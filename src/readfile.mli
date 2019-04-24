open Astlib
open Parse_errors

type path = string
  
val parse_file: path -> Ast.file pv
  
val lwt_parse_file: path -> Ast.file pv Lwt.t

