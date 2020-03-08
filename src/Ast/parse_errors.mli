(* Errors *)
type parser_error =
  (* Received string does not match expected string (e.g. end Foo instead of end Bar). *)
  | Mismatch of string * string

  (* Ignored stuff (e.g. 'y' somewhere in the file).
   * (msg, n)
   * msg: error message. N is the number of ignored TOKENS. *)
  | Ignored of string * int

  (* Missing stuff (e.g. missing body *)
  | Missing of string

  | Misplaced of string

  (* Bad symbol (e.g. , instead of ;) *)
  | Bad_symbol of string

  (* Something is unexpectedly empty. e.g. "empty procedure" *)
  | Empty of string
      
  | Not_long_identifier

  (* A .ads file cannot be found *)
  | Cannot_find of Idents.long_ident

val err2s: parser_error -> string

type lp_error = parser_error Loc.loc

val lp2s: lp_error -> string

exception Syntax_error of lp_error

(* Raise syntax_error *)
val syntax_error: Loc.pos -> parser_error -> 'a

(* A (parsing) value and an accumulator of parsing errors. *)
type 'a pv =
  { pv: 'a ;
    errors: lp_error list }

(*** Parse-error monad ***)

(* Returns a value with/without errors. *)
val pv: ?err:lp_error -> 'a -> 'a pv

val map: 'a pv -> ('a -> 'b) -> 'b pv

val bind:    'a pv -> ('a -> 'b pv) -> 'b pv
val (>>=):   'a pv -> ('a -> 'b pv) -> 'b pv
val (let>=): 'a pv -> ('a -> 'b pv) -> 'b pv
val (let>>): 'a pv -> ('a -> 'b) -> 'b pv

val (and>=): 'a pv -> 'b pv -> ('a * 'b) pv

(* Lists *)
val (>>::): 'a pv -> 'a list pv -> 'a list pv
val pnil: 'a list pv

val l_map: 'a list pv -> ('a -> 'b pv) -> 'b list pv

(* Unit *)
val punit: unit pv
val (>>>): unit pv -> 'a pv -> 'a pv
    
val unitjoin: ('a -> unit pv) -> 'a list -> unit pv

(* val (>>+) agglomerates *)
open Loc

(* Switch *)

val swloc: 'a pv loc -> 'a loc pv
val swopt: 'a pv option -> 'a option pv   
val swlist: 'a pv list -> 'a list pv

val swpair1: ('a * 'b) pv -> ('a pv * 'b)
                             
    
