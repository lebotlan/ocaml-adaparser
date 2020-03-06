open Astlib
open Ast
open Parse_errors
open Idents
    
(* Keeps track of opened packages and imported names 
 * (use Package) *)

type path = string

type includedirs = path list

type use_env

val use_env2s: use_env -> string

(* Empty use environment.
 * The file list is necessary to preload the .ads files. 
 * Alternatively, one may provide a list of packages (considered as with directives).
 * The result contains the errors found while parsing .ads files. 
 *
 * init: may share the init's environment's caches. 
 * (If init is specified, the includirs passed as a second argument are expected to be == to the includirs in the shared env). *)
val empty_use_env: ?share:use_env -> includedirs -> ?packages:(long_ident list) -> Ast.file list -> use_env pv Lwt.t

(* Use package *)
val insert_use: use_env -> long_ident -> use_env

(* Finds if an id is bound in some opened packages. 
 * Returns the list of bindings : (qualified name, corresponding declaration). 
 * The list is empty => the id is unknown. 
 * The list has more than 1 element => the id is overloaded. *)
val use_env_binds: use_env -> loc_ident -> (long_ident * declaration) list

(* Returns a package content. *)
val lwt_read_ads: use_env -> long_ident -> (loc_ident * declaration) list pv Lwt.t
  

(* Convenience *)
val empty_package: loc_ident -> declaration

