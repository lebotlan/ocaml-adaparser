open Lexing


(*** Standard, non-lwt version. ***)

type feeder = lexbuf -> Parser.token

(* The parser needs the lexbuf (and passes it to the feeder function). 
 * Args: filename, file in channel *)
val new_lexer: string -> in_channel -> lexbuf * feeder

(* Dumps n lexems to stdout. *)
val dumplex: feeder -> lexbuf -> int -> unit
  

(*** Lwt version ***)

type lwt_feeder = lexbuf -> Parser.token Lwt.t

val lwt_new_lexer: string -> Lwt_io.input_channel -> (lexbuf * lwt_feeder) Lwt.t

val lwt_dumplex: lwt_feeder -> lexbuf -> int -> unit Lwt.t

