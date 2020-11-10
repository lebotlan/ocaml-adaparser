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

(* TODO FIXME empty package... search fixme *)


type found_id =
  { qualified: long_ident ;
    ads_file: file ;
    ads_decl: declaration ;

    adb_file: file option ;
    adb_decl: declaration option }

type pack =
  { found_pack: found_id ;
    defs: (loc_ident * found_id) list }

type pck_env =
  { includedirs: includedirs ;

    (* A cache for file paths (filename => full file path) 
     * None => the file could not be found. *)
    path_cache: (string, path option) Hashtbl.t ;

    (* A cache for package contents. *)
    pack_cache: pack Hashli.t ;

    (* When reseting *)
    reinit: pck_env ;
    
    (* Current state: uses *)
    opened: pack list }

let opened p = p.opened

let get_includes env = env.includedirs

let reset_pck_env env = env.reinit

let pack2s a = Printf.sprintf "package %s ∋ %s"
    (li2s a.found_pack.qualified) (Common.sep (fun (i,_) -> l2s i) ", " a.defs)

let pck_env2s ?(caches=false) env =
  "----- Opened -----\n" ^
  (Common.sep pack2s "\n" env.opened) ^

  if caches then
    "\n\n----- path_cache -----\n" ^
    (Hashtbl.fold (fun fn patho acu -> Printf.sprintf "%s  %s -> %s" acu fn (match patho with None -> "none" | Some p -> p)) env.path_cache "") ^
    
    "\n\n----- pack_cache -----\n" ^
    (Hashli.fold (fun li p acu -> Printf.sprintf "%s  %s => %s" acu (li2s li) (li2s p.found_pack.qualified)) env.pack_cache "") ^
    "\n\n"

  else ""
  
(*
(* empty package, used to introduce packages like A or B in A.B.C. *)
let empty_package_decl name =
  Package { package_name = name ;
            package_sig = true ;
            package_declarations = pv [] ;
            package_comments = [] ;
            package_init = None }

let empty_pack name =
  { found_pack = { qualified = name ;
                   ads_file = Asttool.empty_file (Idents.pack2filename name) ;
                   ads_decl = empty_package_decl name ;
                   adb_file = None ;
                   adb_decl = None } ;
    defs = [] }
*)
    
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

(* Filters public definitions in a package,
 * and also get the object id.  
 * Some definitions export multiple ids. *) 
let filter_public d = match d with
  | Withclause _ -> []
  | Rename _ -> []
  | Packnew (i, _, _) -> [ (i,d) ]
  | Package _ -> []
  (*  | Package { package_name ; _ } -> Some (package_name, d) *)
  | Procdef def -> [ (def.decl.procname, d) ]
  | Procdecl decl -> [ (decl.procname, d) ]
  | Funrename fr -> [ (fr.fun_alias.procname, d) ]
  | Vardef v -> [ (v.varname, d) ]
  | Subtype { st_name = i ; _ } -> [ (i,d) ]

  (* Enumeration *)
  | Typedef { t_name = i ; t_args = [] ; t_constrain = None ; t_body = Enumerate idents } ->
    (i,d) :: List.map (fun v -> (v, d) ) idents

  | Typedef { t_name = i ; _ } -> [ (i,d) ]


(* Inserts, if not already there, the child package into the parent package. *)
let insert_child_pack ~child_pack ~child_id env parent_lid =
  assert (Hashli.mem env.pack_cache parent_lid) ;
  let parent_pack = Hashli.find env.pack_cache parent_lid in

  match i_assoc_opt child_id parent_pack.defs with
  | None ->
    (* Add the child package to the list *)
    Hashli.replace env.pack_cache parent_lid { parent_pack with defs = (child_id, child_pack.found_pack) :: parent_pack.defs }
    
  | Some fid ->
    (* Check that it is indeed the expected package *)
    assert (long_equal fid.qualified child_pack.found_pack.qualified)

(* Filters a list of declarations (usually, a package content):
 * removes with and use, in order to keep only the 'package' declaration *)
let filter_decls dl =
  let filter = function
    | Withclause _ -> false
    | _ -> true
  in
  List.filter filter dl

(* Put the given package into the cache.
 * Returns a unit pv  *)
let rec lwt_cache_pack env pack_name =

  let filename = Idents.pack2filename pack_name in
  let adsfile = filename ^ ".ads"
  and adbfile = filename ^ ".adb" in

  (* Is it already stored? *)
  if Hashli.mem env.pack_cache pack_name then Lwt.return punit
  else
    (* Find ads *)
    match%lwt find_file env adsfile  with
    (* .ads file not found *)  (* TODO FIXME empty package  *)
    | None -> Lwt.return (pv ~err:{ pos = get_li_pos pack_name ; v = Cannot_find pack_name } ())

    | Some path ->
      let%lwt p_adsfile = Readfile.lwt_parse_file path in

      (* Get all errors *)
      let pu1 = Astread.all_errors p_adsfile in

      let%lwt ads_decl = match filter_decls p_adsfile.pv.content.pv with
        | [] -> assert false (* empty file *)
        | [ Package _ as decl ] -> Lwt.return decl
        | _ -> Lwtplus.myfail "Package_env: file %s does not contain a package declaration" path
      in

      (* Find adb *)
      let%lwt adb_file, adb_decl, pu2 = match%lwt find_file env adbfile with
        | None -> Lwt.return (None,None,punit)
        | Some p ->
          try%lwt
            let%lwt p_adbfile = Readfile.lwt_parse_file p in
            let pu2 = Astread.all_errors p_adbfile in

            let%lwt adb_decl = match filter_decls p_adbfile.pv.content.pv with
              | [] -> assert false (* empty file *)
              | [ Package _ as decl ] -> Lwt.return decl
              | _ -> Lwtplus.myfail "Package_env: file %s does not contain a package body" p
            in
            
            Lwt.return (Some p_adbfile.pv, Some adb_decl, pu2)

          with e ->
            let err = mkdummy adbfile (Unexpected_exn e) in
            Lwt.return (None,None, pv ~err ())
      in

      let found_pack =
        { qualified = pack_name ;
          ads_file = p_adsfile.pv ;
          ads_decl ;          
          adb_file ;
          adb_decl }
      in

      (* Get all declarations in ads and adb *)
      let ads_defs = Common.rev_concat_map filter_public (Astread.package_decls found_pack.ads_file).pv
          
      and adb_defs = match adb_file with
        | None -> None
        | Some f -> Some (Common.rev_concat_map filter_public (Astread.package_decls f).pv)
      in

      (* Build a foundid for each def. *)
      let defs = Common.mymap ads_defs
          begin fun (i,decl) ->
            let decl_in_adb = Common.option_mapo adb_defs (fun ldefs -> i_assoc_opt i ldefs) in
            
            (i, { qualified = pack_name @ [i] ;
                  ads_file  = p_adsfile.pv ;
                  ads_decl  = decl ;
                  adb_file ;
                  adb_decl  = decl_in_adb } )
          end
      in

      let pack = { found_pack ; defs } in
      Hashli.add env.pack_cache pack_name pack ;

      let%lwt pu3 = create_parents ~child_pack:pack env (List.rev pack_name) in
      
      Lwt.return (pu1 >>> pu2 >>> pu3)

(* When a package A.B.C is inserted, also insert parents : A and A.B 
 * In addition, the child is added to its parent if not already there. 
 * (The child pack name is reversed) *)
and create_parents ~child_pack env = function
  | [] -> assert false (* no package name *)
  | [_] -> Lwt.return punit (* Finished *)
  | child_id :: rest ->
    let parent_lid = List.rev rest in
    let%lwt pu1 = lwt_cache_pack env parent_lid in
    insert_child_pack ~child_pack ~child_id env parent_lid ;    
    Lwt.return pu1

let preload env pu file =

  let withclauses = Astread.all_wclauses (pv file) in

  (* Feed cache for future USEs.
   * All USEd packages appear necessarily in a WITH clause.
   * Beware : with A.B.C   use A, use B   (B is not the package name, it is A.B)  
   *)
  let preload_clause pu = function
    | Use _ -> Lwt.return pu
    | Usetype _ -> Lwt.return pu
    | With li ->
      let%lwt pu2 = lwt_cache_pack env li in
      Lwt.return (pu >>> pu2)
  in

  Lwt_list.fold_left_s preload_clause pu withclauses

let insert_use env li =

  (* Remove from the list *)
  let opened = List.filter (fun p -> not (Idents.long_equal li p.found_pack.qualified)) env.opened in

  (* Find it *)
  if Hashli.mem env.pack_cache li then
    let pack = Hashli.find env.pack_cache li in
    { env with opened = pack :: opened }
    
  else
    begin
      Printf.printf "Package_env. current env is :\n%s\n%!" (pck_env2s ~caches:true env) ;
      failwith (Printf.sprintf "Package_env.insert_use: package %s not found in environment." (li2s li))
    end
  
let empty_pck_env ?share includedirs ?(packages=[]) files =
  let (path_cache, pack_cache) =
    match share with
    | None -> Hashtbl.create 100, Hashli.create 100
    | Some e ->
      assert (e.includedirs == includedirs) ; (* The cache is meaningful if the paths are the same (and in the same order). *)
      e.path_cache, e.pack_cache
  in

  let rec env0 = { includedirs ;
                   path_cache ;
                   pack_cache ;
                   reinit = env0 ;
                   opened = [] }
  in
  
  (* Include the given packages & stdlib *)
  let%lwt pu1 = Lwt_list.fold_left_s
      (fun pua li ->
         let%lwt pub = lwt_cache_pack env0 li in
         Lwt.return (pua >>> pub))
      
      punit (li_stdlib :: packages)
  in

  (* Open stdlib, copy to reinit. *)
  let env1 = insert_use env0 li_stdlib in
  let rec env2 = { env1 with reinit = env2 } in
   
  (* Preload all packages that appear in files. 
     (Return only errors) *)
  let%lwt pu2 = Lwt_list.fold_left_s (preload env2) pu1 files in

  Lwt.return (pu2 >>> (pv env2))

let pck_find env li =
  match Hashli.find_opt env.pack_cache li with
  | None -> failwith ("Package_env.pck_find: cannot find package " ^ Idents.li2s li)
  | Some p -> p

let pck_env_ibinds env n =      
  let filter pack = Common.revmapfilter pack.defs (fun (i, fid) -> if Idents.equal n i then Some fid else None) in
  List.flatten (List.map filter env.opened)

let find_li env li =

  (* It it a package? *)
  match Hashli.find_opt env.pack_cache li with
  | Some pack -> [ pack.found_pack ]
  | None ->
    (* Is it package.name ? *)
    begin match List.rev li with
      | [] | [_] -> []
      | i :: rli ->
        begin match Hashli.find_opt env.pack_cache (List.rev rli) with
          | None -> [] (* Unknown *)
          | Some pack -> Common.revmapfilter pack.defs (fun (n, fid) -> if Idents.equal n i then Some fid else None)            
        end
    end
  

(*
(* find_longest_prefix env rev_li *)
let rec find_longest_prefix env = function
  | [] | [_] -> None
  | entry :: rest ->
    (* Is rest a (reversed) package name? *)
    let rname = List.rev rest in
    begin match List.find_opt (fun pack -> Idents.long_equal rname pack.found_pack.qualified) env.opened with
      | Some pack -> Some (pack, entry)
      | None -> find_longest_prefix env rest (* No, iterate *)
    end

let pck_env_binds env = function
  | [] -> assert false
  | [i] -> pck_env_ibinds env i
  | li ->
    (* Find package: the longest prefix that matches a package name. *)
    begin match find_longest_prefix env (List.rev li) with
      | None -> []
      | Some (pack, entryname) -> Common.revmapfilter pack.defs (fun (i,d) -> if Idents.equal entryname i then Some d else None)
    end
*)
