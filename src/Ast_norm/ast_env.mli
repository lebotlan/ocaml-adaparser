open Astlib
open Ast
open Idents
    
(* AST Environment: maps identifiers (loc_ident) to declarations. *)

(* Type of an identifier in the environment *)
type id_typ =
  (* This element corresponds to this declaration.
   *  Vardef, Funrename, Rename, etc. *)
  | Decl of declaration

  (* For identifier *)
  | Forid

  (* When identifier (e.g. naming an exception). *)
  | Whenid

  (* Function argument *)
  | Arg of arg

(* An env is basically a mapping from loc_ident to id_typ *)
type env

val empty_env: env

(* Environment containing some builtin names. *)
val builtin_env: env

(* Inserts a binding in the environment. 
 * Replace if already bound and may_replace is true (true by default), otherwise it fails. *)
val insert_env: ?may_replace:bool -> env -> loc_ident -> id_typ -> env

val env_find: env -> loc_ident -> id_typ option

val id_typ2s: id_typ -> string

val env2s: env -> string