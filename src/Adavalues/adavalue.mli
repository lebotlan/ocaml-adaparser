(* Runtime values.
 *   (we consider first-class functions) *)

type id = string

(* 'a is the type of function content (expressions - definitions are replaced by a declare block). *)
type 'a adavalue =
  | Uninitialized

  (* Char, int, floats are enumerations *)
  (* Strings are arrays of chars. *)

  (* Element of a discrete enumeration *)
  | Enum of Simenum.discrete

  (*  Element of a dense enumeration *)
  | Enumd of Simenum.dense

  (* Array *)
  | Array of 'a adavalue Simarray.t

  (* Record *)
  | Record of 'a adavalue Simrecord.t

  (* Syntactic function or procedure. *)
  | Fun of 'a funvalue

  (* Builtin function *)
  | Builtin of (string * (Adaenv.env -> ('a adavalue) list -> (Adaenv.env * 'a adavalue)))

and 'a funvalue =
  { fname: id ;
    args: 'a arg list ;
    content: 'a }

and 'a arg =
  { arg_name: id ;
    arg_default: 'a adavalue option }

(* Representation of Types *)
and 'a adatype = {
  typename: string ;

  (* Default value of this type *)
  default: 'a adavalue ;

  (* Type content *)
  tcontent: 'a tcontent }

and 'a tcontent =
  | TRecord of 'a adatype Simrecord.rtype
  | TArray of 'a adatype Simarray.atype
  | TEnum of Simenum.discrete_typ
  | TEnumd of Simenum.dense_typ


val tos: ?margin:string -> 'a adavalue -> string

val ttos: 'a adatype -> string

(* Check if a value belongs to the given type. *)
val belongs: 'a adatype -> 'a adavalue -> bool

(* Builtins *)
val t_int: 'a adatype
val t_bool: 'a adatype
val t_char: 'a adatype
val t_float: 'a adatype
val t_string: 'a adatype
val t_unit: 'a adatype

(* Fails if this is not an int. *)
val get_int: 'a adavalue -> int
val get_float: 'a adavalue -> float
val get_string: 'a adavalue -> string
val get_bool: 'a adavalue -> bool

val mk_int: int -> 'a adavalue
val mk_bool: bool -> 'a adavalue
val mk_char: char -> 'a adavalue
val mk_float: float -> 'a adavalue
val mk_string: string -> 'a adavalue

val unit: 'a adavalue

