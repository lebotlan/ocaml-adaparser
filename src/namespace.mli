open Ast
open Idents

(* Renaming environment *)
type env_rename

(* Environment containing renamings and USE clauses. *)
type namespace =
  private { renamings: env_rename ;
            use: long_ident list }

val init_namespace: namespace

val insert_pr: pack_rename -> namespace -> namespace
val insert_use: long_ident -> namespace -> namespace

(* Apply namespace to declaration *)
val applynm_procdecl: namespace -> procdecl -> procdecl

(* val applynm_expr: namespace -> expr -> expr *)

(* ident_complies ref i   indicates if i is equal to ref when the namespace is taken into account. 
 * ref must be an 'absolute' identifier (no namespace used). *)
val ident_complies: ref:long_ident -> (namespace * long_ident) -> bool
  
