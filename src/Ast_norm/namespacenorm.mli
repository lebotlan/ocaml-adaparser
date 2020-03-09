open Astlib
open Parse_errors
open Ast
    

(* This module should eventually totally replace Namespace. 
 * Beware - checking functions signature can be done even if the .ads files are not found. *)



(*** Normalize: 
 *  - replaces all identifiers by their fully qualified name. 
 *  - remove package renames (which are now useless). 
 *  - USE clauses are kept, since some overloaded identifiers may be kept unqualified (we could remove useless USE clauses).
 *)


(* Namespace: contains renamings, USE clauses, and local variables. *)
type nmspace

val nmspace2s: nmspace -> string

val get_env: nmspace -> Ast_env.env
val get_use_env: nmspace -> Use_env.use_env

type path = string

(* Normalize a file.
 * includedirs: directories in which ads files are looked for.
 * pv: contains only errors found while reading ads files. 
 *
 * Note: pv could possibly contain errors found while normalizing the namespace (unknown packages, unknown identifiers). 
 *)
val n_file: includedirs:path list -> file pv -> file pv Lwt.t


(* Like n_file, but also returns a list of procedure / functions  with their associated namespace. *)
val all_procdecl: includedirs:path list -> file pv -> (file pv * (nmspace * procdef) list) Lwt.t

