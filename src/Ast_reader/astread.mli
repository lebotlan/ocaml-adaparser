open Astlib
open Ast
open Parse_errors
open Namespace
    
val all_errors: Ast.file pv -> unit pv

(* Used by xmprepare plugin - old style *)
val all_procdecl: Ast.file pv -> (namespace * procdecl) list



(* All With/use clauses *)
val all_wclauses: Ast.file pv -> withclause list

(* Identifiers exported by a package (.ads,.adb) *)
val package_decls: Ast.file -> pl_declarations
    
(* Get all procdefs in a file. New style *)
val all_procdefs: Ast.file pv -> procdef list
    
