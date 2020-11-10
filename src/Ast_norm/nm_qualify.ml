
(* Qualify: expand all expandable names (aliases, implicit packages). *)

open Astlib
open Ast
open Idents
open Parse_errors
open Loc
open Astreader
open Astmap
open Loc_env
open Package_env


(* When expanding names, we replace a loc_ident or a long_ident
 * by a Qualified or a long_ident.
 * We keep track of what has been replaced. *)
type replaced =
  (* R_qualified (original, replacement) *)
  | R_qualified of loc_ident * core_expr

  (* R_longid (original, replacement) *)
  | R_longid of long_ident * long_ident

(* The key to record replacements is:
 *   - the full replaced longid (longid case)
 *   - or the first Id in a Qualified. *)
type replace_key =
  | K_qualified of loc_ident
  | K_longid of long_ident

let replacekey2s = function
  | K_qualified l -> "Q[" ^ l2s l ^ "]"
  | K_longid li -> "L[" ^ li2s li ^ "]"

let replaced2s = function
  | R_qualified (l, e) -> l2s l ^ " -> " ^ Astprint.core_expr2s ~margin:"" e
  | R_longid (li1, li2) -> li2s li1 ^ " => " ^ li2s li2

let replace_expr_equal e1 e2 = match (e1, e2) with
  | Id i1, Id i2 -> equal i1 i2
  | Qualified (li1, i1), Qualified (li2, i2) -> long_equal li1 li2 && equal i1 i2
  | _ -> false

let replaced_equal r1 r2 = match (r1, r2) with
  | R_qualified (i1, e1), R_qualified (i2, e2) -> equal i1 i2 && replace_expr_equal e1 e2

  (* li1a and li2a are not always equal: li2a can be the qualified version of li1a *)
  | R_longid (_li1a, li1b), R_longid (_li2a, li2b) -> (* long_equal li1a li2a && *) long_equal li1b li2b
  | _ -> false

let get_key_loc = function
  | K_qualified l -> l.pos
  | K_longid l -> get_li_pos l

(* Filters package renames. 
 * We keep USE clauses, because some overloaded identifiers may still be unqualified. *)
let remove_decl = function
  | Rename _ -> false
  | Funrename _ -> false
  | _ -> true

module K_tbl = Hashtbl.Make
    (struct
      type t = replace_key
      let equal k1 k2 = match (k1,k2) with
        | K_qualified i1, K_qualified i2 -> Idents.equal i1 i2
        | K_longid li1, K_longid li2 -> Idents.long_equal li1 li2
        | _ -> false

      let hash = function
        | K_qualified i -> Hashtbl.hash i.v
        | K_longid li -> Idents.hashli li
    end)

class qenv init_env init_use ~var_prefix =
  object(self:'s)

    inherit Nmspace.nmenv init_env init_use
    
    (* Keep track of identifiers that were expanded:
     *  replace_key => replaced
     *
     * This field contains persistent data (according to Astmap spec),
     * however, since it is mutable, we may still consider 
     * the full acu as scoped (the assoc field remains unchanged, but its content is updated. 
     * ...dangerous play with side-effects... ?
     *)
    val assoc = (Hashtbl.create 1000 : (replace_key, replaced) Hashtbl.t)

    val var_count = 1
    val var_subst = []

    method get_var_count = var_count
    method get_var_subst = var_subst

    (* Var def *)
    method new_var i =
      let newname = { pos = i.pos ; v = prefix (var_prefix ^ string_of_int var_count ^ "_") i.v } in
      (newname, {< var_count = var_count + 1 ; var_subst = (i,newname) :: var_subst >})      

    method rename_var i =
      match Idents.i_assoc_opt i var_subst with
      | Some name -> Some [name]
      | None ->
        Printf.printf "Nm_qualify: unknown variable in var_subst (was it a variable defined in a package ?) : '%s'\n%!" (l2s i) ;
        None
    
    method! block_exit (outacu:'s)     = {< var_count = outacu#get_var_count >}
    method! merge_pre = self
    method! merge_mid (acu1:'s)        = {< var_count = acu1#get_var_count >}
    method! merge_end (_:'s) (acu2:'s) = {< var_count = acu2#get_var_count >}
    
    method! tos =
      Printf.sprintf "\n----- Namespace -----\n%s\n\n%s\n\nAssoc :\n%s\n--------------------------\n\n"
        (env2s self#get_decl_env) (pck_env2s self#get_pck_env)
        (Hashtbl.fold (fun key rep acu -> acu ^ "    " ^ replacekey2s key ^ " : " ^ replaced2s rep ^ "\n") assoc "")

    (* Receives a loc ident. Replace it by a (relocated) qualified name. 
     * Returns Some longid
     * warn: if we must warn when an identifier is unknown. *)
    method qualify_id ~warn i =
      
      (* Printf.printf "\n\n ==> Qualify %s\nCurrent env is :\n%s\n%!" (l2s i) (self#tos) ;  *)
      
      (* Is it a name in the current environment? *)
      match env_find self#get_decl_env i with
      | Some (Forid | Whenid | Arg _) -> None (* Do nothing *) 
      | Some (Decl (Withclause _ | Packnew _ | Package _ | Typedef _ | Subtype _ | Procdef _ | Procdecl _)) ->
        (* Known identifier, we return it as is, so that it becomes Qualified *)
        Some [i]
        
      | Some (Decl (Vardef _)) -> self#rename_var i
      | Some (Decl (Rename pr)) -> Some (reloc i.pos pr.pack_orig)
      | Some (Decl (Funrename fr)) -> Some (reloc i.pos fr.fun_orig)
      | None ->     
        (* Unknown identifier.
         * Is it a visible identifier in some opened package? *)
        begin match pck_env_ibinds self#get_pck_env i with
          | [ foundid ] -> Some (reloc i.pos foundid.qualified)
          | [] ->
            (* Unknown identifier *)            
            if warn then Printf.printf "*Warning: unknown identifier %s (%s)\n%!" (l2s i) (pos2s i.pos) ;
            None
            
          | _ ->
            (* Overloading. We choose NOT to expand. *)
            Printf.printf "*Warning: overloaded identifier %s\n%!" (l2s i) ;
            None
        end

    method insert_assoc key replaced =
      if Hashtbl.mem assoc key then
        begin
          (* Sometimes an identifier is duplicated (e.g. X,Y : Integer), hence we can replace it several times. 
           * It should however, be replaced in the same way. *)
          let old_replace = Hashtbl.find assoc key in
          if replaced_equal replaced old_replace then ()
          else
            Printf.printf "*Error* An identifier was found twice : %s (%s) but replaced inconsistently : %s vs %s \n%!"
              (replacekey2s key) (pos2s (get_key_loc key)) (replaced2s replaced) (replaced2s old_replace)
        end ;
      Hashtbl.replace assoc key replaced ;
      self

    method qualify_longid ~warn li =
      match li with
      | [] -> assert false (* Empty long ident *)
      | i :: is ->
        (* Printf.printf "#qualify_longid %s(%s) -> #qualify_id\n%!" (l2s i) (pos2s i.pos) ; *)
        begin match self#qualify_id ~warn i with
          | None -> return li self (* Nope *)
          | Some li2 ->
            let full_li = li2 @ is in
            return full_li (self#insert_assoc (K_longid full_li) (R_longid (li, full_li)))
        end

  end

(* Mapper that inserts fully qualified identifiers in the ast. *)
class ['a] qmap (userfun: 'a user_fun) =
  object
    constraint 'a = #qenv
      
    inherit ['a] Nmspace.envmap userfun as super

    (* Filters package & function renames. *)
    method! pl_declarations kind dlp acu =
      let+ pl = super#pl_declarations kind dlp acu in
      let>> ll = pl in
      List.filter remove_decl ll
    
    (*** Expand identifiers. ***)

    (* Qualify 1st of longid identifier if applicable. *)
    method! long_id li acu = acu#qualify_longid ~warn:true li


    method! vardef kind v acu =
      match kind with
      | In_package -> super#vardef kind v acu
      | Toplevel | In_declare | In_proc ->
        (* We record the original name.. *)
        let { rval = vd ; acu = acu2 } = super#vardef kind v acu in

        (* Then substitute (the previous name is in the environment). *)
        let (i2, acu3) = acu2#new_var vd.varname in
        
        return { vd with varname = i2 } acu3
        
    (* #arg is invoked twice (see astmap). 
     * It should not be a problem to qualify twice the identifiers, otherwise rewrite method arg here. *)
    (*
    method! typename li acu = self#long_id li acu
    method! arg kind a acu = super#arg kind a acu
    *)
        
    (* expr_id are the first elements when calling a function such as P1.P2.P3.fun 
     * P1 is an expr_id, P2, P3, fun are qualifiedors. 
     * P1 can be expanded, not P2, P3, fun. 
     * If expanded, it must be replaced by a Qualified. *)
    method! core_expr pos ln e acu = match ln.label_nm, e with
      (* Do not expand argument labels (A) *)      
      | S, Id i ->        
        (*        Printf.printf "\nConsidering Expr_id(%s)  current ACU is \n%s\n\n%!" (l2s i) (acu#tos) ; *)
        (*        Printf.printf "#core_expr %s(%s) -> #qualify_id\n%!" (l2s i) (pos2s i.pos) ; *)
        
        begin match acu#qualify_id ~warn:true i with
          | None -> return e acu (* Nope *)
          | Some [] -> assert false
          | Some ((i1 :: _) as li) ->
            (* Printf.printf " -> Some %s\n\n%!" (li2s li) ; *)
            let e2 = Asttool.li2expr li in
            return e2.v (acu#insert_assoc (K_qualified i1) (R_qualified (i, e2.v)))
        end

      | _ ->
        begin
          (* Recursive calls *)
          let re = super#core_expr pos ln e acu in
          let acu = re.acu in

          match re.rval with

          (* See if A.B should be replaced by a qualified. *)
          | Select ( { v = Qualified (li1,i1) ; _ } , i2) ->

            (* Check that li1.i1 is a package which contains i2. *)
            begin match Package_env.find_li acu#get_pck_env (li1 @ [ i1 ; i2 ]) with
              | [] -> re (* Nope *)

              (* Even if overloaded, the qualified id is the same. *)
              | _foundid :: _ -> return (Qualified (li1 @ [i1], i2)) acu
            end

          | _ -> re            
        end


  end

(* Init environment: empty local env, empty package env. *)
let init_nmspace includedirs ~var_prefix list =
  let%lwt puse = Package_env.empty_pck_env includedirs list in
  Lwt.return
    (let>> use = puse in
     (new qenv Loc_env.empty_env use ~var_prefix)#init)
  
let n_file ~includedirs ~var_prefix file =
  try%lwt
    let%lwt pinit_acu = init_nmspace includedirs ~var_prefix [file.pv] in
    let pu = pv ~cperr:pinit_acu ()
    and acu = pinit_acu.pv in
    
    Lwt.return
      (let norm_file = (new qmap acu#userfun)#file file acu in        
       (pu, norm_file.rval))

  with e ->
    Lwt_io.printf "Exception in nm_qualify with file %s\n" file.pv.path ;%lwt
    Lwt.fail e
    
