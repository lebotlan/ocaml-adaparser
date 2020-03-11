open Astlib
open Parse_errors
open Ast
open Ast_env
open Use_env
open Astreader.Astmap
open Idents
    
(*** Normalize: 
 *  - replaces all identifiers by their fully qualified name. 
 *  - remove package renames (which are now useless). 
 *  - USE clauses are kept, since some overloaded identifiers may be kept unqualified (we could remove useless USE clauses).
 *)

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


(* Qualify environment. *)
class qenv: env -> use_env ->
  object ('s)
    inherit Nmspace.nmenv
    method insert_assoc: replace_key -> replaced -> 's
    method qualify_longid: warn:bool -> long_ident -> (long_ident, 's) ret
    method qualify_id: warn:bool -> loc_ident -> long_ident option
  end

class ['a] qmap: 'a user_fun ->
  object
    inherit ['a] tree_mapper
    constraint 'a = #qenv
  end
  

(* Normalize a file.
 * includedirs: directories in which ads files are looked for.
 * pv: contains only errors found while reading ads files. 
 *
 * Note: pv could possibly contain errors found while normalizing the namespace (unknown packages, unknown identifiers). 
 *)
val n_file: includedirs:path list -> file pv -> file pv Lwt.t


(* Like n_file, but also returns a list of procedure / functions  with their associated namespace. *)
val all_procdecl: includedirs:path list -> file pv -> (file pv * (qenv * procdef) list) Lwt.t

