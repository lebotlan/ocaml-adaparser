open Astlib
open Ast
open Idents
open Parse_errors
open Loc
open Adaparser
open Astreader
open Astmap
    
type path = string

type includedirs = path list

type ads_defs =
  { pack_name: long_ident ;
    defs: loc_ident list }

type nmspace =
  { pack_renames: pack_rename list ;
    use: ads_defs list ;
    local_defs: loc_ident list }

let (//) = Filename.concat

let not_found pack_name =
  { pv = { pack_name ;
           defs = [] } ;
    errors = [ { pos = get_li_pos pack_name ;
                 v = Cannot_find pack_name } ]
  }

let init_nmspace =
  { pack_renames = [] ;
    use = [] ;
    local_defs = [] }

let ads_defs2s ads = Printf.sprintf "package %s exports %s" (li2s ads.pack_name) (Common.sep l2s ", " ads.defs)

let nmspace2s nms =
  Printf.sprintf "---- Namespace ----\n%s\n\n%s\n\n  Local defs : %s\n\n"
    (Common.sep (Astprint.packrenames ~margin:"") "\n" nms.pack_renames)
    (Common.sep ads_defs2s "\n" nms.use)
    (Common.sep l2s ", " nms.local_defs)

let insert_ads nms ads = { nms with use = ads :: nms.use }

let insert_local nms li = { nms with local_defs = li :: nms.local_defs }

(* Cache of absolute paths *)
let path_cache = Hashtbl.create 100

let find_file includedirs name =
  let key = (includedirs, name) in
  
  if Hashtbl.mem path_cache key then Lwt.return (Hashtbl.find path_cache key)
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
      
      let%lwt opath = loop includedirs in      
      Hashtbl.add path_cache key opath ;
      Lwt.return opath
    end

(* Maps a long_ident package name to a file name. *)
let pack2file li =
  let open Text in
  
  let res = Text.lower (Common.sep l2s "-" li) ^ ".ads" in  
  let len = length res in

  (* Special rule for a- g- i- s-, see spec. *)
  match sub res 0 2 with
  | "a-" | "g-" | "i-" | "s-" -> get res 0 ^ "~" ^ sub res 2 (len - 2)
  | _ -> res


(* Filters public definitions in a package *)
let filter_public = function
  | Withclause _ -> None
  | Rename _ -> None
  | Packnew (i, _, _) -> Some i
  | Package _ -> None
  | Typedef (i, _, _, _) -> Some i
  | Subtype (i, _, _) -> Some i
  | Procdef def -> Some def.decl.procname
  | Procdecl decl -> Some decl.procname
  | Funrename fr -> Some fr.fun_alias.procname
  | Vardef v -> Some v.varname


(* Cache of ads declarations *)
let ads_cache = Hashtbl.create 100                  

let lwt_read_ads includedirs pack_name =

  let filename = pack2file pack_name in
  let key = (includedirs, filename) in
  
  if Hashtbl.mem ads_cache key then Lwt.return (Hashtbl.find ads_cache key)
  else  
    match%lwt find_file includedirs filename with
    (* .ads file not found *)
    | None -> Lwt.return (not_found pack_name)

    | Some path ->
      let%lwt p_file = Readfile.lwt_parse_file path in

      Lwt.return
        (p_file >>=
         (fun f ->
            (* get all decls *)
            Astread.package_decls f >>=
            (fun decls ->
               let defs = Common.revmapfilter decls filter_public in
               let result = pv { pack_name ; defs } in
               Hashtbl.add ads_cache key result ;
               result
            )))


let cached_ads includedirs pack_name =
  let filename = pack2file pack_name in
  let key = (includedirs, filename) in
  
  if Hashtbl.mem ads_cache key then Hashtbl.find ads_cache key
  else failwith "Namespacenorm.cached_ads: not cached."


(* Replace, if needed, a long identifier by a fully qualified long identifier. *)
let qualified_li li acu =
  Printf.printf "Qualified_li li = %s, current acu = \n%s\n%!" (li2s li) (nmspace2s acu) ;
  li (*  ???? TODO or not TODO ?????? *)

(* Mapper that inserts fully qualified identifiers. *)
let qualified_ids_map includedirs =
  object
    inherit [nmspace] tree_mapper as super

    method! upacu inacu _ = inacu

    (*
    (* Example, to be removed *)
    method! use_id li acu = return li { acu with nmsp = insert_use li acu.nmsp }
    method! pack_rename pr acu = return pr { acu with nmsp = insert_pr pr acu.nmsp }
    method! procdecl decl acu = return decl (insert_decl decl acu)
    (* *)
*)

    (* Update namespace *)

    (* Qualify identifier if applicable. *)
    method! long_id li acu = return (qualified_li li acu) acu 
        
    (* Update namespace ? *)
    method! use_id li acu =
      let qual_packname = qualified_li li acu in
      let p_defs = cached_ads includedirs qual_packname in
      return qual_packname (insert_ads acu p_defs.pv)

    method! pack_rename pr acu =
      Printf.printf "Visiting %s\n%!" (Astprint.packrenames ~margin:"  " pr) ;
      let ret = super#pack_rename pr acu in
      return ret.rval { ret.racu with pack_renames = pr :: ret.racu.pack_renames } 

    method! var_id id acu =
      Printf.printf "Visiting vardef %s\n%!" (l2s id) ;
      return id (insert_local acu id)
    
    (*
    (* Vardef *)
*
    (* Typedef *)
    method type_id: (loc_ident, 'a) mapper

    
    (* Beware, expr_id work with Select. *)
    (* method expr_id: label_namespace -> (loc_ident, 'a) mapper *)
    (* method select_id: (loc_ident, 'a) mapper *)


    method pnew_id: (loc_ident, 'a) mapper
    method procdecl: (procdecl, 'a) mapper
    method procdef: (procdef, 'a) mapper
    method procname: (loc_ident, 'a) mapper
    method pv_declaration: (declaration list pv, 'a) mapper
*)
  end

let n_file includedirs file =
  (* Put all USE ads files into the cache. *)
  let withclauses = Astread.all_wclauses (pv file) in

  (* Feed cache *)
  let preload = function
    | With _ -> Lwt.return punit
    | Use li ->
      let%lwt p_ads_defs = lwt_read_ads includedirs li in
      Lwt.return { pv = () ;
                   errors = p_ads_defs.errors }
      
    | Usetype _ -> Lwt.return punit      
  in
  
  let%lwt errs = Lwt_list.map_s preload withclauses in

  (* Get only errors *)
  let errs = swlist errs in
  
  let norm_content = (qualified_ids_map includedirs)#content file.content init_nmspace in
  let norm_file = { file with content = norm_content.rval } in

  Lwt.return { pv = norm_file ;
               errors = errs.errors }
    
 
