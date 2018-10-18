{
  (* This is an approximate Ada 2005 lexer. *)

  (* Notes : 
   *   - it does not support unicode nor latin-1 encodings. 
   *   - for the application I had in mind, this lexer (and parser) read only valid Ada files,
   *     hence we take some liberty with respect to the Ada spec.
   *)

  (* As stated by the Ada 2005 specification:
   *   A lexical element is either a delimiter, an identifier, a reserved word, a numeric_literal,
   *   a character_literal, a string_literal, or a comment. *)

module type TLEX =
sig
  type state
  type 'a t
      
  val init: state    
  val next_token: state -> Lexing.lexbuf -> (Parser.token * state) t
end
  
module type MONAD =
sig
  type 'a t
  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val fail : exn -> 'a t
  val on_refill : Lexing.lexbuf -> unit t
end

open Parser
open Astlib
open Idents

(* Get position in file. *)
let getpos lexbuf = Lexing.lexeme_start_p lexbuf

(* Count a new line *)
let nline lexbuf  = Lexing.new_line lexbuf

(***  Errors  ***)
exception Lexing_Error of string * Lexing.position

let forbidden c lexbuf = Lexing_Error (Printf.sprintf "Forbidden character '%c'" c, getpos lexbuf)

module Make (M: MONAD) =
  struct

    type 'a t = 'a M.t
    
    (* "Monadic" refill pre-hook. *)
    let refill_hook k lexbuf = M.bind (M.on_refill lexbuf) (fun () -> k lexbuf)

    (*** Lexer state ***)
    type state = {
      (* List of comments read so far. *)
      comments : string list ;
    }

    (* Initial lexer state *)
    let init = {
      comments = [] ;
    }

    (* Keywords (as usual) but also delimiters are stored in a hashtable. *)
    let keywords = Hashtbl.create 100
    let add k v  = Hashtbl.add keywords (norm k) v

    (* Given a string s, returns an IDENTifier, a keyword, or a delimiter. *)
    let find_keyword must_exist s =
      let s = norm s in
      try Hashtbl.find keywords s
      with Not_found ->
        if must_exist then 
          (* A symbol is matched but not found in the table. *)
          assert false 
        else IDENT s

    let () =
      (* Keywords *)

      (* Some Ada 2005 keywords are considered like standard identifiers. *)
      
      (* add "ABORT" ABORT ; *)   (* add "ABS" ABS ; *)       add "ABSTRACT" ABSTRACT ;   add "ACCEPT" ACCEPT ;
      add "ACCESS" ACCESS ;       add "ALIASED" ALIASED ;     (* add "ALL" ALL ; *)       add "AND" AND ;
      add "ARRAY" ARRAY ;         (* add "AT" AT ; *)         add "BEGIN" BEGIN ;         add "BODY" BODY ;
      add "CASE" CASE ;
      add "CONSTANT" CONSTANT ;   add "DECLARE" DECLARE ;     add "DELAY" DELAY ;         add "DELTA" DELTA ;
      add "DIGITS" DIGITS ;       add "DO" DO ;               add "ELSE" ELSE ;           add "ELSIF" ELSIF ;
      add "END" (END []) ;        add "ENTRY" ENTRY ;         add "EXCEPTION" EXCEPTION ; add "EXIT" EXIT ;
      add "FOR" FOR ;             add "FUNCTION" FUNCTION ;   add "GENERIC" GENERIC ;     add "GOTO" GOTO ;
      add "IF" IF ;               add "IN" IN ;               add "IS" IS ;               add "LIMITED" LIMITED ;
      add "LOOP" LOOP ;           add "MOD" MOD ;             add "NEW" NEW ;             add "NOT" NOT ;
      add "NULL" NULL ;           add "OF" OF ;               add "OR" OR ;               add "OTHERS" OTHERS ;
      add "OUT" OUT ;             add "PACKAGE" PACKAGE ;     add "PRIVATE" PRIVATE ;
      add "PROCEDURE" PROCEDURE ; add "PROTECTED" PROTECTED ; add "RAISE" RAISE ;         add "RANGE" RANGE ;
      add "RECORD" RECORD ;       add "REM" REM ;             add "RENAMES" RENAMES ;     add "REQUEUE" REQUEUE ;
      add "RETURN" RETURN ;       add "REVERSE" REVERSE ;     add "SELECT" SELECT ;       add "SEPARATE" SEPARATE ;
      add "SUBTYPE" SUBTYPE ;     add "TAGGED" TAGGED ;       add "TASK" TASK ;           add "TERMINATE" TERMINATE ;
      add "THEN" THEN ;           add "TYPE" TYPE ;           add "UNTIL" UNTIL ;         add "USE" USE ;
      add "WHEN" WHEN ;           add "WHILE" WHILE ;         add "WITH" WITH ;           add "XOR" XOR ;

      (* Delimiters *)
      add "." DOT ;               add "<" LT ;                add "(" LPAREN ;            add "+" PLUS ;
      add "|" BAR ;               add "&" AMPAND ;            add "*" STAR ;              add ")" RPAREN ;
      add ";" SEMI ;              add "-" MINUS ;             add "/" SLASH ;             add "," COMMA ;
      add ">" GT ;                add ":" COLON ;             add "=" EQUAL ;             add "'" TICK ;     
      add ".." DOTDOT ;           add "<<" LTLT ;             add "<>" BRAKET ;           add "<=" LEQ ;
      add "**" STARSTAR ;         add "/=" NOTEQ ;            add ">>" GTGT ;             add ">=" GEQ ;
      add ":=" ASSIGN ;           add "=>" IMPLY ;            add "'RANGE" TICKRANGE ;

      ()
}

(* Regular expressions *)

let eolchar         = ['\n' '\r']
let eol             = (eolchar | "\r\n")
let space           = [' ' '\t']

let digit           = ['0'-'9']

(* Digit in whatever base *)
let extended_digit  = ['0'-'9' 'a'-'z' 'A'-'Z']

let numeral         = digit( '_'? digit)*
                      let exponent        = ['e' 'E'] ('+'?|'-') numeral
let decimal_literal = numeral('.'?numeral)? exponent?
let base            = numeral
let based_numeral   = extended_digit('_'?extended_digit)*
                      let based_literal   = base '#' based_numeral ('.'based_numeral)? '#' exponent?

let letter          = ['a'-'z' 'A'-'Z']
let alpha           = letter | digit
let identifier      = letter('_'? alpha)*

let quot            = '\039' (* Simple quote *)
let dquot           = '\034' (* Double quote *)

(* In ocamllex, # means 'except' *)
let any             = _ # eolchar

let stringchar      = any # dquot
let string_lit      = (dquot dquot | stringchar)*

(* PRAGMA *)
let pragma          = ['P' 'p'] ['R' 'r'] ['A' 'a'] ['G' 'g'] ['M' 'm'] ['A' 'a']
let pragma_line     = pragma any*

(* RANGE *)
let tick_range      = "'" space* ['R' 'r'] ['A' 'a'] ['N' 'n'] ['G' 'g'] ['E' 'e']

refill {refill_hook}

(* function NEXT_TOKEN *)
rule next_token state = parse

  (* Space and pragma lines are ignored. *)
  | space | pragma_line                          { next_token state lexbuf }

  (* Identifier and keywords *)
  | identifier as s                              { match find_keyword false s with                   
                                                   | END _ -> M.return (END (List.rev state.comments), { comments = [] })
                                                   | x     -> M.return (x, state) }

  (* 'RANGE is simpler to handle in the lexer than in the parser. *)
  | tick_range as s                              { M.return (find_keyword true s, state) }

  (* Delimiters *)
  | ("." | "<" | "(" | "+" |
     "|" | "&" | "*" | ")" |
     ";" | "-" | "/" | "," |
     ">" | ":" | "=" | "'" |
     ".." | "<<" | "<>" | "<=" |
     "**" | "/=" | ">>" | ">=" |
     ":=" | "=>") as s                           { M.return (find_keyword true s, state) }

  (* Numeric literal *)
  | (decimal_literal | based_literal) as s       { M.return (NUM (s, String.contains s '.'), state) }

  (* Character literal *)
  | quot (any as c) quot                         { M.return (CHAR c, state) }

  (* String literal *)
  | dquot (string_lit as s) dquot                { M.return (STRING s, state) }

  (* Comment *)
  | '-' '-' (any* as c)                          { next_token { comments = c :: state.comments } lexbuf }

  (* New line *)
  | eol                                          { nline lexbuf ; next_token state lexbuf }

  (* End of file *)
  | eof                                          { M.return (EOF (List.rev state.comments), { comments = [] }) }

  (* Unknown character *)
  | _ as c                                       { M.fail (forbidden c lexbuf) }

{
end    
}

