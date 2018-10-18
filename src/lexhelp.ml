open Astlib
open Parser
open Lexer
open Lexing

type feeder = lexbuf -> Parser.token
type lwt_feeder = lexbuf -> Parser.token Lwt.t

module No_monad =
struct
  type 'a t = 'a
  let return x = x
  let bind x f = f x
  let fail e = raise e
  let on_refill _ = () (* Nothing to do, the lexbuf.refill function will be invoked anyway. *)
end

module Stdlex = Make(No_monad)

(* Creates a new context-dependent lexer *)
let new_lexer path chin =
  (* This is the lexer-state call stack. *)
  let state = ref Stdlex.init in

  let lexbuf = Lexing.from_channel chin in 
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = path } in
  
  let feeder lexbuf =
    let (value, newstate) = Stdlex.next_token !state lexbuf in
    state := newstate ;
    value
  in

  (lexbuf, feeder)


let lwt_new_lexer path chin =

  (* We need an ad-hoc buffer & refill function which are lwt-compliant. *)
  
  (* State of our ad-hoc buffer. *)
  let size = 2048 in
  let buf = Bytes.create size
  and pos = ref 0
  and len = ref 0
  in

  (* Function used to build the lexbuf (using from_function).
   * refill_buffer should feed buf' with at most n bytes. *)
  let refill_buffer buf' n =

    (* on_refill has been invoked right before, 
     * hence there must be available bytes (unless EOF is reached). *)
    let available = !len - !pos in
    if available <= 0 then 0
    else
      begin
        let n' = min available n in
        Bytes.blit buf !pos buf' 0 n';
        pos := !pos + n';
        n'
      end
  in

  let lexbuf = Lexing.from_function refill_buffer in
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = path } in
  
  let module Lwt_monad =
  struct
    type 'a t = 'a Lwt.t
    let return = Lwt.return
    let bind = Lwt.bind
    let fail e = Lwt.fail e
    let on_refill lexb =
      assert%lwt (lexb == lexbuf) ;%lwt
      (* Move remaining bytes to the beginning. *)
      let available = !len - !pos in
      let () =
        Bytes.blit buf !pos buf 0 available ;
        pos := 0 ;
        len := available ;
      in

      (* Try to read more bytes, inserted at pos2. *)
      let pos2 = available in
      let%lwt nread = Lwt_io.read_into chin buf pos2 (size - pos2) in
      let () = len := available + nread in
      (* Lwt_io.printf " lwt-read %d bytes.\n" nread ;%lwt *)
      
      Lwt.return_unit
  end
  in

  
  let module Lwtlex = Make(Lwt_monad) in
  
  let state = ref Lwtlex.init in
  
  let feeder lexbuf =
    let%lwt (value, newstate) = Lwtlex.next_token !state lexbuf in
    let () = state := newstate in
    Lwt.return value
  in

  Lwt.return (lexbuf, feeder)
  

    
let value2s = function
  | DOT -> "."
  | SEMI -> ";"
  | XOR -> "XOR"
  | WITH -> "WITH"
  | WHILE -> "WHILE"
  | WHEN -> "WHEN"
  | USE -> "USE"
  | UNTIL -> "UNTIL"
  | TYPE -> "TYPE"
  | TICK -> "'"
  | TICKRANGE -> "'RANGE"
  | THEN -> "THEN"
  | TERMINATE -> "TERMINATE"
  | TASK -> "TASK"
  | TAGGED -> "TAGGED"
  | SUBTYPE -> "SUBTYPE"
  | STARSTAR -> "**"
  | STAR -> "*"
  | SLASH -> "SLASH"
  | SEPARATE -> "SEPARATE"
  | SELECT -> "SELECT"
  | RPAREN -> ")"
  | REVERSE -> "REVERSE"
  | RETURN -> "RETURN"
  | REQUEUE -> "REQUEUE"
  | RENAMES -> "RENAMES"
  | REM -> "REM"
  | RECORD -> "RECORD"
  | RANGE -> "RANGE"
  | RAISE -> "RAISE"
  | PROTECTED -> "PROTECTED"
  | PROCEDURE -> "PROCEDURE"
  | PRIVATE -> "PRIVATE"
  | PRAGMA -> "PRAGMA"
  | PLUS -> "+"
  | PACKAGE -> "PACKAGE"
  | OUT -> "OUT"
  | OTHERS -> "OTHERS"
  | OR -> "OR"
  | OF -> "OF"
  | NULL -> "NULL"
  | NOTEQ -> "NOTEQ"
  | NOT -> "NOT"
  | NEW -> "NEW"
  | MOD -> "MOD"
  | MINUS -> "-"
  | LTLT -> "<<"
  | LT -> "<"
  | LPAREN -> "("
  | LOOP -> "LOOP"
  | LIMITED -> "LIMITED"
  | LEQ -> "<="
  | IS -> "IS"
  | IN -> "IN"
  | IMPLY -> "=>"
  | IF -> "IF"
  | GTGT -> ">>"
  | GT -> ">"
  | GOTO -> "GOTO"
  | GEQ -> ">="
  | GENERIC -> "GENERIC"
  | FUNCTION -> "FUNCTION"
  | FOR ->  "FOR"
  | EXIT -> "EXIT"
  | EXCEPTION -> "EXCEPTION"
  | EQUAL -> "="
  | ENTRY -> "ENTRY"
  | ELSIF -> "ELSIF"
  | ELSE -> "ELSE"
  | DOTDOT -> ".."
  | DO -> "DO"
  | DELAY -> "DELAY"
  | DELTA -> "DELTA"
  | DIGITS -> "DIGITS"
  | DECLARE -> "DECLARE"
  | CONSTANT -> "CONSTANT"
  | COMMA -> ","
  | COLON -> ":"
  | CASE -> "CASE"
  | BRAKET -> "BRAKET"
  | BODY -> "BODY"
  | BEGIN -> "BEGIN"
  | BAR -> "|"
  | ASSIGN -> ":="
  | ARRAY -> "ARRAY"
  | AND -> "AND"
  | AMPAND -> "&"
  | ALIASED -> "ALIASED"
  | ACCESS -> "ACCESS"
  | ACCEPT -> "ACCEPT"
  | ABSTRACT -> "ABSTRACT"
  | ABS -> "ABS"
  | STRING s -> "\"" ^ s ^ "\""
  | NUM (s,_) -> s
  | IDENT i -> Idents.i2s i
  | EOF _ -> "EOF"
  | END _ -> "END"
  | CHAR c -> "'" ^ String.make 1 c ^ "\'"
    
let rec dumplex lexer lexbuf n =
  if n <= 0 then Printf.printf "\n\n%!" 
  else
    begin
      Printf.printf "%s " (value2s (lexer lexbuf)) ;
      dumplex lexer lexbuf (n-1)
    end

let rec lwt_dumplex lexer lexbuf n =
  if n <= 0 then Lwt_io.printf "\n\n"
  else
    begin
      let%lwt v = lexer lexbuf in
      Lwt_io.printf "%s " (value2s v) ;%lwt
      lwt_dumplex lexer lexbuf (n-1)
    end
