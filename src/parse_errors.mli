(* Errors *)
type parser_error =
  (* Received string does not match expected string (e.g. end Foo instead of end Bar). *)
  | Mismatch of string * string

  (* Ignored stuff (e.g. 'y' somewhere in the file). 
   * Error message. N is the number of ignored TOKENS. *)
  | Ignored of string * int

  (* Missing stuff (e.g. missing body *)
  | Missing of string

  (* Something is unexpectedly empty. e.g. "empty procedure" *)
  | Empty of string
      
  | Not_long_identifier

val err2s: parser_error -> string

type lp_error = parser_error Loc.loc

exception Syntax_error of lp_error

(* Raise syntax_error *)
val syntax_error: Loc.pos -> parser_error -> 'a

(* A (parsing) value and an accumulator of parsing errors. *)
type 'a pv =
  { pv: 'a ;
    errors: lp_error list }

val lperr2s: margin:string -> lp_error list -> string

(*** Parse-error monad ***)

(* Returns a value with/without errors. *)
val pv: ?err:lp_error -> 'a -> 'a pv

val map: 'a pv -> ('a -> 'b) -> 'b pv

val bind: 'a pv -> ('a -> 'b pv) -> 'b pv
val (>>=): 'a pv -> ('a -> 'b pv) -> 'b pv

(* Lists *)
val (>>::): 'a pv -> 'a list pv -> 'a list pv
val pnil: 'a list pv

val p_map: 'a list pv -> ('a -> 'b pv) -> 'b list pv

(* Unit *)
val punit: unit pv
val (>>>): unit pv -> unit pv -> unit pv
val unitjoin: ('a -> unit pv) -> 'a list -> unit pv

(* val (>>+) agglomerates *)
open Loc
    
val swloc: 'a pv loc -> 'a loc pv

val swopt: 'a pv option -> 'a option pv
    
val swlist: 'a pv list -> 'a list pv
    
