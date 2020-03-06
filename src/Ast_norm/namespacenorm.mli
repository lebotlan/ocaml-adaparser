open Astlib
open Parse_errors
open Ast
    

(* This module should eventually totally replace Namespace. 
 * Beware - checking functions signature can be done even if the .ads files are not found. *)



(*** Normalise namespace: replaces all identifiers by their fully qualified name. ***)


(* Namespace: contains renamings, USE clauses, and local variables. *)
type nmspace

val nmspace2s: nmspace -> string


type path = string

(* Normalize a file.
 * includedirs: directories in which ads files are looked for.
 * pv: contains only errors found while reading ads files. 
 *
 * Note: pv could possibly contain errors found while normalizing the namespace (unknown packages, unknown identifiers). 
 *)
val n_file: includedirs:path list -> file pv -> file pv Lwt.t



(* => Return a list of functions with their associated namespaces? *)

