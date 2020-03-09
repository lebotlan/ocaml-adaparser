open Ast
open Parse_errors

val packrenames: margin:string -> pack_rename -> string

val funrenames: margin:string -> fun_rename -> string

val declaration2b: margin:string -> declaration -> string

val procdef2b: margin: string -> procdef -> string

val arg2s: arg -> string

val procdecl2s: procdecl -> string

val pfile2s: file pv -> string

val expr2s: margin:string -> expr -> string

val core_expr2s: margin:string -> core_expr -> string
  
