open Astlib
open Ast
open Loc_env
open Package_env
open Astreader.Astmap
open Idents
    
(* Namespace: contains renamings, USE clauses, local variables in the current scope,
 * as well as all proc declarations. *)
class nmenv: decl_env -> pck_env ->
  object ('s)

    (* Invoke first. 
     * Injects the opened packages of pck_env into the namespace (see insert_with). *)
    method init: 's
    
    method tos: string

    (* All proc defs found until now, with their associated environment. *)
    method get_defs: ('s * procdef) list

    (* Insert a procedure definition *)
    method insert_def: 's * procdef -> 's

    (* Get current loc_env *)
    method get_decl_env: decl_env

    (* Get current package_env. *)
    method get_pck_env: pck_env

    (* Inserts a definition in local environment.
     * Package renames are put here too. *)
    method insert_env: loc_ident -> id_typ -> 's
      
    method insert_use: long_ident -> 's

    (* With A.B.C  binds the name A to the content of package A.
     * and so when a value such as A.f1 is encountered, we know that A is a package present in package_env. *)
    method insert_with: long_ident -> 's
    
    (* Used by astmap. *)
    method block_exit: 's -> 's
    method merge_pre : 's
    method merge_mid : 's -> 's
    method merge_end : 's -> 's -> 's
    method userfun: 's user_fun        
  end


(* Environment that can be extended. 
 * It keeps track of the current namespace (loc_env and packages). *)
class ['a] envmap: 'a user_fun ->
  object
    inherit ['a] tree_mapper
    constraint 'a = #nmenv
  end
  
