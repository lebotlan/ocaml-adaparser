open Astlib
open Ast
open Parse_errors
open Idents
    
(* Records known packages (with) and keeps track of imported names (use).
 * Inserts stdlib by default. 
 *
 *  1 - Init the environment with empty_pck_env: it preloads all packages.
 *  2 - Then, the only operation that changes the environment is USE (insert_use)
 *  3 - Read operations: what is short-id ? (unqualified)
 *                       what is long-id ?  (qualified)
 *
 *      Read operations work with types, functions, procedures, but also with subpackages: what is A.B.C ?, what is A ?
 *
 *      Subpackages can be explicit in one package (package bla is ...)
 *                  or they can be defined in another file (gada.text_io)
 *                  we consider both cases equivalent. (Not guaranteed it is the official Ada semantics. Whatever.)
 *)

type path = string
type includedirs = path list

type pck_env

val get_includes: pck_env -> includedirs
val pck_env2s: ?caches:bool -> pck_env -> string

(* Empty pck environment (contains stdlib, though).
 * The file list is necessary to preload all the *.ads and, if possible *.adb files.
 * Alternatively, one may provide a list of packages, considered as 'with' directives, so that files are pre-fetched.
 *
 * The result contains the errors found while parsing .ads,.adb files. 
 *
 * share: used to share the init's environment's caches. 
 * (If share is specified, the includirs passed as a second argument are expected to be == to the includirs in the shared env). 
 *)
val empty_pck_env: ?share:pck_env -> includedirs -> ?packages:(long_ident list) -> Ast.file list -> pck_env pv Lwt.t

(* Copy the given pck_env, keeps its config, caches, but reset its content (no use). *)
val reset_pck_env: pck_env -> pck_env

(* Use package. *)
val insert_use: pck_env -> long_ident -> pck_env

(* Result when looking for an id. *)
type found_id =
  { (* Qualified name *)
    qualified: long_ident ;

    (* Found in this ads file. *)
    ads_file: file ;

    (* Corresponding declaration *)
    ads_decl: declaration ;

    (* If the adb is found, also return its full definition. *)
    adb_file: file option ;
    
    adb_decl: declaration option }


type pack =
  { (* Package declaration and definition, as a found_id record. *)
    found_pack: found_id ;

    (* List of identifiers publicly defined in this package. 
     * In case of subpackages, the ads_file is not necessarily the current pack ads file. *)
    defs: (loc_ident * found_id) list }

(* Note: when a declaration with A.B.C is found, a subpackage B is added to the definitions of A. *)

(* List of currently opened packages. *)
val opened: pck_env -> pack list

(* Find package id *)
val pck_find: pck_env -> long_ident -> pack

val find_li: pck_env -> long_ident -> found_id list

(* Finds if a (short) id is bound in some opened packages. (implicit bind)
 * Returns the list of bindings.
 * The list is empty => the id is unknown. 
 * The list has more than 1 element => the id is overloaded. *)
val pck_env_ibinds: pck_env -> loc_ident -> found_id list

(* Finds if a fully qualified longid is known (known package, known id).
 * Returns a list - in case the identifier is overloaded in the given package. 
 * Empty list => the longid is unknown. 
 * We return the longest prefix that matches a value without accessing inner record fields. 
 * Hence, found_id is a prefix of the long_ident (or the long_ident itself). 
 *
 * The long_ident can be a single ident, then pck_env_ibinds is invoked. *)
(* val pck_env_binds: pck_env -> long_ident -> found_id list *)




(* Returns a package spec content. *)
(* val lwt_read_ads: pck_env -> long_ident -> ((loc_ident * declaration) list * file option) pv Lwt.t *)

(* Convenience: makes a declaration for an empty package (e.g. Ada) *)
(* val empty_package: loc_ident -> declaration *)

