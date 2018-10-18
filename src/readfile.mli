open Astlib

val parse_file: string -> Ast.file
  
val lwt_parse_file: string -> Ast.file Lwt.t



