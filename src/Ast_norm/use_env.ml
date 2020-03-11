open Astlib
open Idents
open Ast
open Parse_errors
open Adaparser
open Astreader
open Loc
    
type path = string

type includedirs = path list

let (//) = Filename.concat

type ads =
  { pack_name: long_ident ;

    (* Indicates if this package is already opened with a "use". *)
    opened: bool ;

    (* List of identifiers publicly defined in this package. *)
    defs: (loc_ident * declaration) list }

(* Note: when a declaration with A.B.C is found, an empty subpackage C is added to the definitions of B in the current use_env (not in the cache),
   and an empty subpackage B is added to the definition of A. *)

type use_env =
  { includedirs: includedirs ;

    (* A cache for file paths (filename => full file path) 
     * None => the file could not be found. *)
    path_cache: (string, path option) Hashtbl.t ;

    (* A cache for ads content. The key is the file name.
     * All ads in the cache are closed (not opened). *)
    ads_cache: (string, ads) Hashtbl.t ;

    (* Current state. *)
    ads: ads list }
    

let ads2s a = Printf.sprintf "package %s %s %s"
    (li2s a.pack_name) (if a.opened then "exports " else "may contain") (Common.sep (fun (i,_) -> l2s i) ", " a.defs)

let use_env2s env = Common.sep ads2s "\n" env.ads

(* Given a filename, finds where it is (in includedirs). *)
let find_file env name =
  if Hashtbl.mem env.path_cache name then Lwt.return (Hashtbl.find env.path_cache name)
  else
    begin
      (* Look in all include dirs *)
      let rec loop = function
        | [] -> Lwt.return_none (* Not found *)
        | dir :: rest ->
          let path = dir // name in
          if%lwt Lwtfile.is_file ~follow:true path then Lwt.return_some path
          else loop rest
      in
      
      let%lwt opath = loop env.includedirs in
      Hashtbl.add env.path_cache name opath ;
      Lwt.return opath
    end

(* Filters public definitions in a package *)
let filter_public d = match d with
  | Withclause _ -> None
  | Rename _ -> None
  | Packnew (i, _, _) -> Some (i,d)
  | Package _ -> None
  | Typedef { t_name = i ; _ } -> Some (i,d)
  | Subtype { st_name = i ; _ } -> Some (i,d)
  | Procdef def -> Some (def.decl.procname, d)
  | Procdecl decl -> Some (decl.procname, d)
  | Funrename fr -> Some (fr.fun_alias.procname, d)
  | Vardef v -> Some (v.varname, d)

(* Take the defs associated to name in the environment. 
 * Update them: apply function fupd (and possibly open the package). *)
let update_defs opened env name fupd =

  let rec loop = function
    | [] -> [ { pack_name = name ; opened ; defs = fupd [] } ]
    | d1 :: ds ->
      if Idents.long_equal d1.pack_name name then
        { d1 with defs = fupd d1.defs ; opened = opened || d1.opened } :: ds
      else d1 :: loop ds
  in
  { env with ads = loop env.ads }

let same_name i (i2, _) = Idents.equal i i2

(* Insert defs not already in the list. *)
let insert_use_ads env ads =
  update_defs true env ads.pack_name
    (fun defs -> List.rev_append ads.defs (List.filter (fun (d1,_) -> not (List.exists (same_name d1) ads.defs)) defs))

(* empty subpackage, to be added to ads content, in order to make the existence of subpackages explicit. *)     
let empty_package name =
  Package { package_name = [name] ;
            package_sig = true ;
            package_declarations = pv [] ;
            package_comments = [] ;
            package_init = None }

let insert_subpackage env aa = function
  | [] -> env (* Empty path, do not record it. *)
  | li -> update_defs false env li (fun defs -> if List.exists (same_name aa) defs then defs else (aa, empty_package aa) :: defs)

(* Put the given package into the cache.
 * Returns a unit pv  *)
let lwt_cache_ads env pack_name =

  let filename = Idents.pack2file pack_name in

  if Hashtbl.mem env.ads_cache filename then Lwt.return punit
  else  
    match%lwt find_file env filename with
    (* .ads file not found *)
    | None -> Lwt.return (pv ~err:{ pos = get_li_pos pack_name ; v = Cannot_find pack_name } ())

    | Some path ->
      let%lwt p_file = Readfile.lwt_parse_file path in

      Lwt.return
        (* get all decls *)
        (let>= f = p_file in
         let>> decls = Astread.package_decls f in

         let defs = Common.revmapfilter decls filter_public in
         let result = { pack_name ; opened = false ; defs } in
         Hashtbl.add env.ads_cache filename result ;
         ())

let lwt_read_ads env pack_name =
  let%lwt pu = lwt_cache_ads env pack_name in
  try
    Lwt.return
      (let>> () = pu in
       let filename = Idents.pack2file pack_name in
       (Hashtbl.find env.ads_cache filename).defs)
  with _ -> failwith ("package " ^  li2s pack_name ^ " cannot be found." )
  

(* Preload ads files into the cache *)
let preload penv file =

  let withclauses = Astread.all_wclauses (pv file) in

  (* Feed cache for future USEs.
   * All USEd packages appear necessarily in a WITH clause.
   * Beware : with A.B.C   use A, use B   (B is not the package name, it is A.B)  
   *)
  let preload_clause pacu = function
    | Use _ -> Lwt.return pacu
    | Usetype _ -> Lwt.return pacu
    | With li ->
      let%lwt errs = lwt_cache_ads pacu.pv li in
      Lwt.return
        (let>= acu = pacu in
      
         (* Insert subpackages in all parent packages.
          *   with  A.B.C.D  : insert B into A,  C into A.B, etc. *)
         let rec loop rpath acu = function
           | [] -> acu
           | aa :: rest ->
             let path = List.rev rpath in
             (* insert_subpackage: if path is empty, do nothing *)
             loop (aa :: rpath) (insert_subpackage acu aa path) rest
         in
         
         { pv = loop [] acu li ;
           errors = errs.errors })
  in

  Lwt_list.fold_left_s preload_clause penv withclauses

let empty_use_env ?share includedirs ?(packages=[]) files =
  let (path_cache, ads_cache) =
    match share with
    | None -> Hashtbl.create 100, Hashtbl.create 100
    | Some e ->
      assert (e.includedirs == includedirs) ; (* The cache is meaningful if the paths are the same (and in the same order). *)
      e.path_cache, e.ads_cache
  in

  let penv = pv { includedirs ;
                  path_cache ;
                  ads_cache ;
                  ads = [] }
  in

  let%lwt penv = Lwt_list.fold_left_s
      (fun penv li -> let%lwt pu = lwt_cache_ads penv.pv li in Lwt.return (let>> () = pu in penv.pv))
      penv packages
  in

  (* Preload all packages that appear in files. 
     (Return only errors) *)
  Lwt_list.fold_left_s preload penv files


let cached_ads env pack_name =
  let filename = pack2file pack_name in
  
  if Hashtbl.mem env.ads_cache filename then Hashtbl.find env.ads_cache filename
  else
    begin
      Printf.printf "Namespacenorm.cached_ads: warning not cached: %s (normal for prefixes)\n%!" (li2s pack_name) ;
      { pack_name ; opened = false ; defs = [] }
    end

let insert_use env li = insert_use_ads env (cached_ads env li)

let use_env_binds env i =
  let filter ads =
    if not ads.opened then None
    else
      match List.find_opt (same_name i) ads.defs with
      | None -> None
      | Some (i, d) -> Some (ads.pack_name @ [i], d)
  in
  Common.revmapfilter env.ads filter
