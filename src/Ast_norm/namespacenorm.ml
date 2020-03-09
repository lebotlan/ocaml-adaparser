
(* Namespacenorm: normalise namespace.
 * Expand all expandable names (aliases, implicit packages).
 * Characterize identifiers (arguments, variables, for-loop identifiers, ...)
 *)

open Astlib
open Ast
open Idents
open Parse_errors
open Loc
open Astreader
open Astmap
open Ast_env
open Use_env

type path = string


(* When expanding names, we replace a loc_ident or a long_ident
 * by a Select or a long_ident.
 * We keep track of what has been replaced. *)
type replaced =
  (* R_select (original, replacement) *)
  | R_select of loc_ident * core_expr

  (* R_longid (original, replacement) *)
  | R_longid of long_ident * long_ident

(* The key to record replacements is:
 *   - the full replaced longid (longid case)
 *   - or the first Id in a Select. *)
type replace_key =
  | K_select of loc_ident
  | K_longid of long_ident

type nmspace =
  { (* Environment : variables, arguments, ...*)
    env: env ;

    (* Opened packages (use) *)
    use: use_env ;

    (* All procdefs. *)
    defs: (nmspace * procdef) list ;
    
    (* Keep track of identifiers that were expanded:
     *  replace_key => replaced
     *
     * This field contains persistent data,
     * however, since it is mutable, we may still consider 
     * the full acu as scoped (the assoc field remains unchanged, but its content is updated. 
     *)
    assoc: (replace_key, replaced) Hashtbl.t }

let get_env nm = nm.env
let get_use_env nm = nm.use

let replacekey2s = function
  | K_select l -> "S[" ^ l2s l ^ "]"
  | K_longid li -> "L[" ^ li2s li ^ "]"

let replaced2s = function
  | R_select (l, e) -> l2s l ^ " -> " ^ Astprint.core_expr2s ~margin:"" e
  | R_longid (li1, li2) -> li2s li1 ^ " => " ^ li2s li2

let get_key_loc = function
  | K_select l -> l.pos
  | K_longid l -> get_li_pos l

let nmspace2s nms =
  Printf.sprintf "\n----- Namespace -----\n%s\n\n%s\n\nAssoc :\n%s\n--------------------------\n\n"
    (env2s nms.env) (use_env2s nms.use)
    (Hashtbl.fold (fun key rep acu -> acu ^ "    " ^ replacekey2s key ^ " : " ^ replaced2s rep ^ "\n") nms.assoc "")

let insert_assoc nms key replaced =
  if Hashtbl.mem nms.assoc key then
    begin
      (* We cannot possibly have already replaced this localized identifier !?! *)
      Printf.printf "Identifier found twice : %s (%s)    replacement : %s \n%!" (replacekey2s key) (pos2s (get_key_loc key)) (replaced2s replaced)
    end ;
  Hashtbl.add nms.assoc key replaced ;
  nms

let reloc pos li = List.map (fun i -> { i with pos } ) li

(* Receives a loc ident. Replace it by a (relocated) qualified name. *)
let qualify ~warn i nms =

  (* Is it a name in the current environment? *)
  match env_find nms.env i with
  | Some (Forid | Whenid | Arg _) -> None (* Do nothing *) 
  | Some (Decl (Withclause _ | Packnew _ | Package _ | Typedef _ | Subtype _ | Procdef _ | Procdecl _ | Vardef _)) -> None (* Do nothing *)
  | Some (Decl (Rename pr)) -> Some (reloc i.pos pr.pack_orig)
  | Some (Decl (Funrename fr)) -> Some (reloc i.pos fr.fun_orig)
  | None ->     
    (* Unknown identifier.
     * Is it a visible identifier in some opened package? *)
    begin match use_env_binds nms.use i with
      | [ (li, _) ] -> Some (reloc i.pos li)
      | [] ->
        (* Unknown identifier *)
        if warn then Printf.printf "*Warning: unknown identifier %s\n%!" (l2s i) ;
        None
        
      | _ ->
        (* Overloading. We choose NOT to expand. *)
        Printf.printf "*Warning: overloaded identifier %s\n%!" (l2s i) ;
        None
    end

let expand_longid ~warn li acu =
  match li with
  | [] -> assert false (* Empty long ident *)
  | i :: is ->
    begin match qualify ~warn i acu with
      | None -> return li acu (* Nope *)
      | Some li2 ->
        let full_li = li2 @ is in
        return full_li (insert_assoc acu (K_longid full_li) (R_longid (li, full_li)))
    end
    
let userfun =
  let merge ~acu0 f =
    let (acu1, g) = f ~acu1:acu0 in
    let (acu2, c) = g ~acu2:{ acu0 with defs = acu1.defs } in

    let acu = { acu0 with defs = acu2.defs } in    
    (acu, c)

  and block_exit inacu outacu = { inacu with defs = outacu.defs } in

  { block_exit ; merge }


(* Filters package renames. 
 * We keep USE clauses, because some overloaded identifiers may still be unqualified. *)
let remove_decl = function
  | Rename _ -> false
  | Funrename _ -> false
  | _ -> true

(* Mapper that inserts fully qualified identifiers in the ast. *)
let qualified_ids_map =
  object(self)
    inherit [nmspace] tree_mapper userfun as super

    (* With => the first package name is now bound in the environment. *)
    method! with_id li acu =
      let- (li, acu) = expand_longid ~warn:false li acu in

      let first = match li with
        | [] -> assert false (* Empty package name ? *)
        | ii :: _ -> ii
      in      
      { acu with env = insert_env acu.env first (Decl (Use_env.empty_package first)) }

    (* Filters package renames. *)
    method! pl_declarations kind dlp acu =
      let+ pl = super#pl_declarations kind dlp acu in
      let>> ll = pl in
      List.filter remove_decl ll
    
    method! use_id li acu =
      let- (li, acu) = self#long_id li acu in
      { acu with use = insert_use acu.use li }

    method! pack_rename pr acu =
      (* long_id is qualified when calling super. *)
      let- (pr, acu) = super#pack_rename pr acu in
      { acu with env = insert_env acu.env pr.pack_alias (Decl (Rename pr)) }

    method! packnew ((id,_,_) as p) acu =
      let- (p, acu) = super#packnew p acu in
      { acu with env = insert_env acu.env id (Decl (Packnew p)) }
        
    method! fun_rename fr acu =
      let- (fr, acu) = super#fun_rename fr acu in
      { acu with env = insert_env acu.env fr.fun_alias.procname (Decl (Funrename fr)) }
    
    (*** Expand identifiers. ***)

    (* Qualify 1st of longid identifier if applicable. *)
    method! long_id li acu = expand_longid ~warn:true li acu

    method! typename li acu = self#long_id li acu

    (* expr_id are the first elements when calling a function such as P1.P2.P3.fun 
     * P1 is an expr_id, P2, P3, fun are selectors. 
     * P1 can be expanded, not P2, P3, fun. 
     * If expanded, it must be replaced by a Select. *)
    method! core_expr pos ln e acu = match e with
      | Id i ->

        (* Printf.printf "\nConsidering Expr_id(%s)  current ACU is \n%s\n\n%!" (l2s i) (nmspace2s acu) ; *)
        
        begin match qualify ~warn:true i acu with
          | None -> return e acu (* Nope *)
          | Some [] -> assert false
          | Some (i1 :: iz) ->
            (* Printf.printf " -> Some %s\n\n%!" (li2s (i1 :: iz)) ;  *)
            let e2 = List.fold_left (fun ee ii -> Select ({ pos ; v = ee }, ii)) (Id i1) iz in
            return e2 (insert_assoc acu (K_select i1) (R_select (i, e2)))
        end
        
      | _ -> super#core_expr pos ln e acu


    (*** Record local names. ***)

    method! for_id id acu = return id { acu with env = insert_env acu.env id Forid }
    method! when_id id acu = return id { acu with env = insert_env acu.env id Whenid }
    
    method! vardef kind vardef acu =
      let- (vd, acu) = super#vardef kind vardef acu in
      { acu with env = insert_env acu.env vd.varname (Decl (Vardef vd)) }

    method! typedef typedef acu =
      let- (td, acu) = super#typedef typedef acu in
      { acu with env = insert_env acu.env td.t_name (Decl (Typedef td)) }

    method! subtypedef subtypedef acu =
      let- (std, acu) = super#subtypedef subtypedef acu in
      { acu with env = insert_env acu.env std.st_name (Decl (Subtype std)) }

    method! arg kind a acu =
      let- (a, acu) = super#arg kind a acu in
      match kind with
      | Typedef_arg | Proc_arg false -> acu (* Not binding *)
      | Proc_arg true -> { acu with env = insert_env acu.env a.argname (Arg a) } (* This is a binder *)

    method! procdef def acu =
      let- (pd, acu) = super#procdef def acu in
      { acu with env = insert_env acu.env pd.decl.procname (Decl (Procdef pd)) ;
                 defs = (acu, pd) :: acu.defs }

  end

let init_nmspace includedirs list =

  let%lwt puse = Use_env.empty_use_env includedirs list in
  
  Lwt.return
    (let>> use = puse in

     { env = builtin_env ;
       defs = [] ;
       use ;
       assoc = Hashtbl.create 1000 } )

let all_procdecl ~includedirs file =
  let%lwt pinit_acu = init_nmspace includedirs [file.pv] in
  Lwt.return
    (swpair1
       (let>> acu = pinit_acu in
        let norm_file = qualified_ids_map#file file acu in        
        norm_file.rval.pv, norm_file.acu.defs ))
  
let n_file ~includedirs file =
  let%lwt (f, _) = all_procdecl ~includedirs file in
  Lwt.return f
