open Astlib
open Ast
open Parse_errors
open Namespace
    
val all_errors: Ast.file pv -> unit pv

(* Used by xmprepare plugin *)
val all_procdecl: Ast.file pv -> (namespace * procdecl) list

(* All With/use clauses *)
val all_wclauses: Ast.file pv -> withclause list

(* Identifiers exported by a package (.ads) *)
val package_decls: Ast.file -> pv_declaration
    
