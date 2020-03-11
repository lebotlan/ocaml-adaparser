
(* Qualify: expand all expandable names (aliases, implicit packages). *)

open Astlib
open Ast
open Idents
open Parse_errors
open Loc
open Astreader
open Astmap
open Ast_env
open Use_env


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

let replacekey2s = function
  | K_select l -> "S[" ^ l2s l ^ "]"
  | K_longid li -> "L[" ^ li2s li ^ "]"

let replaced2s = function
  | R_select (l, e) -> l2s l ^ " -> " ^ Astprint.core_expr2s ~margin:"" e
  | R_longid (li1, li2) -> li2s li1 ^ " => " ^ li2s li2

let get_key_loc = function
  | K_select l -> l.pos
  | K_longid l -> get_li_pos l

(* Filters package renames. 
 * We keep USE clauses, because some overloaded identifiers may still be unqualified. *)
let remove_decl = function
  | Rename _ -> false
  | Funrename _ -> false
  | _ -> true

class qenv env use =
  object(self:'selftype)

    inherit Nmspace.nmenv env use
    
    (* Keep track of identifiers that were expanded:
     *  replace_key => replaced
     *
     * This field contains persistent data,
     * however, since it is mutable, we may still consider 
     * the full acu as scoped (the assoc field remains unchanged, but its content is updated. 
     *)
    val assoc = (Hashtbl.create 1000 : (replace_key, replaced) Hashtbl.t)
      
    method! tos =
      Printf.sprintf "\n----- Namespace -----\n%s\n\n%s\n\nAssoc :\n%s\n--------------------------\n\n"
        (env2s env) (use_env2s use)
        (Hashtbl.fold (fun key rep acu -> acu ^ "    " ^ replacekey2s key ^ " : " ^ replaced2s rep ^ "\n") assoc "")

    (* Receives a loc ident. Replace it by a (relocated) qualified name. *)
    method qualify_id ~warn i =
      (* Is it a name in the current environment? *)
      match env_find env i with
      | Some (Forid | Whenid | Arg _) -> None (* Do nothing *) 
      | Some (Decl (Withclause _ | Packnew _ | Package _ | Typedef _ | Subtype _ | Procdef _ | Procdecl _ | Vardef _)) -> None (* Do nothing *)
      | Some (Decl (Rename pr)) -> Some (reloc i.pos pr.pack_orig)
      | Some (Decl (Funrename fr)) -> Some (reloc i.pos fr.fun_orig)
      | None ->     
        (* Unknown identifier.
         * Is it a visible identifier in some opened package? *)
        begin match use_env_binds use i with
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


    method insert_assoc key replaced =
      if Hashtbl.mem assoc key then
        begin
          (* We cannot possibly have already replaced this localized identifier !?! *)
          Printf.printf "Identifier found twice : %s (%s)    replacement : %s \n%!" (replacekey2s key) (pos2s (get_key_loc key)) (replaced2s replaced)
        end ;
      Hashtbl.add assoc key replaced ;
      self

    method qualify_longid ~warn li =
      match li with
      | [] -> assert false (* Empty long ident *)
      | i :: is ->
        begin match self#qualify_id ~warn i with
          | None -> return li self (* Nope *)
          | Some li2 ->
            let full_li = li2 @ is in
            return full_li (self#insert_assoc (K_longid full_li) (R_longid (li, full_li)))
        end

  end

(* Mapper that inserts fully qualified identifiers in the ast. *)
class ['a] qmap (userfun: 'a user_fun) =
  object(self)
    constraint 'a = #qenv
      
    inherit ['a] Nmspace.envmap userfun as super

    (* Filters package renames. *)
    method! pl_declarations kind dlp acu =
      let+ pl = super#pl_declarations kind dlp acu in
      let>> ll = pl in
      List.filter remove_decl ll
    
    (*** Expand identifiers. ***)

    (* Qualify 1st of longid identifier if applicable. *)
    method! long_id li acu = acu#qualify_longid ~warn:true li

    method! typename li acu = self#long_id li acu

    (* expr_id are the first elements when calling a function such as P1.P2.P3.fun 
     * P1 is an expr_id, P2, P3, fun are selectors. 
     * P1 can be expanded, not P2, P3, fun. 
     * If expanded, it must be replaced by a Select. *)
    method! core_expr pos ln e acu = match e with
      | Id i ->

        (* Printf.printf "\nConsidering Expr_id(%s)  current ACU is \n%s\n\n%!" (l2s i) (nmspace2s acu) ; *)
        
        begin match acu#qualify_id ~warn:true i with
          | None -> return e acu (* Nope *)
          | Some [] -> assert false
          | Some (i1 :: iz) ->
            (* Printf.printf " -> Some %s\n\n%!" (li2s (i1 :: iz)) ;  *)
            let e2 = List.fold_left (fun ee ii -> Select ({ pos ; v = ee }, ii)) (Id i1) iz in
            return e2 (acu#insert_assoc (K_select i1) (R_select (i, e2)))
        end
        
      | _ -> super#core_expr pos ln e acu

  end

let init_nmspace includedirs list =

  let%lwt puse = Use_env.empty_use_env includedirs list in
  
  Lwt.return
    (let>> use = puse in
     new qenv builtin_env use)

let all_procdecl ~includedirs file =
  let%lwt pinit_acu = init_nmspace includedirs [file.pv] in
  Lwt.return
    (swpair1
       (let>> acu = pinit_acu in
        let norm_file = (new qmap acu#userfun)#file file acu in        
        norm_file.rval.pv, norm_file.acu#get_defs ))
  
let n_file ~includedirs file =
  let%lwt (f, _) = all_procdecl ~includedirs file in
  Lwt.return f


