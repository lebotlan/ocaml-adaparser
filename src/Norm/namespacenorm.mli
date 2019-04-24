open Astlib
open Parse_errors
open Idents
open Ast
    

(* This module should eventually totally replace Namespace. 
 * Beware - checking functions signature can be done even if the .ads files are not found. *)



(*** Normalise namespace: replaces all identifiers by their fully qualified name. ***)


(* Namespace: contains renamings, USE clauses, and local variables. *)
type nmspace

(* Definitions found inside a .ads file *)
type ads_defs

val ads_defs2s: ads_defs -> string
val nmspace2s: nmspace -> string

(* Empty namespace *)
val init_nmspace: nmspace

type path = string

(* Directories in which ads file are looked for. *)
type includedirs = path list

(* Cached *)
val find_file: includedirs -> string -> path option Lwt.t

(* In order to avoid parsing twice the same files, a cache is used. 
 * The pv.errors may contain Cannot_find errors for missing ads files, and other parsing errors.  *)
val lwt_read_ads: includedirs -> long_ident -> ads_defs pv Lwt.t

(* If we are guaranteed to find the value in the cache, Lwt is not needed. 
 * raise Failure otherwise. *)
val cached_ads: includedirs -> long_ident -> ads_defs pv

(* Inserts the definition of a .ads file into the current namespace. *)
val insert_ads: nmspace -> ads_defs -> nmspace


(* Normalize a file.
 * pv: errors found while reading ads files. *)
val n_file: includedirs -> file -> file pv Lwt.t


