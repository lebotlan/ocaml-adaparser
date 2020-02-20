open Ast
open Parse_errors

val packrenames: margin:string -> pack_rename -> string

val procdecl2s: procdecl -> string

val pfile2s: file pv -> string
  
