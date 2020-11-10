open Astlib
open Parse_errors
open Ast
open Package_env
    
(*** Normalize: 
 *  - replaces all identifiers by their fully qualified name. Introduces 'Qualified' in the ast everywhere it is possible.
 *  - remove package renames (which are now useless). 
 *  - USE clauses are kept, since some overloaded identifiers may be kept unqualified (we could remove useless USE clauses).
 *  - Variables are renamed to unique identifiers. 
 *)


(* Normalize a file.
 * includedirs: directories in which ads files are looked for.
 * pv: contains only errors found while reading ads files. 
 *
 * Note: the unit pv contains errors found while normalizing the namespace (unknown packages, unknown identifiers). 
 *)
val n_file: includedirs:path list -> var_prefix:string -> file pv -> (unit pv * file pv) Lwt.t
