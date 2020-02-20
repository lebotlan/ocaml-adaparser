(* Simulates an enumeration, including finite ones, infinite ones (Int) and dense ones (Float). *)

(* Represents the type itself (not a value). *)
(* 'a is a phantom type: a_dense or a_discrete *)
type 'a typ

type a_dense    (* Behaves like floats *)
type a_discrete (* Behaves like integers *)

type dense_typ = a_dense typ
type discrete_typ = a_discrete typ

(* Create a finite enumeration type.
 * cs: case sensitive *)
val create_from_list: cs:bool -> name:string -> string list -> discrete_typ

(* Enumeration in bijection with an int interval. *)
val create_from_int_bij: name:string -> (string -> int) -> (int -> string) -> min:int -> max:int -> discrete_typ

(* Builtin enums *)
val bool_enum: discrete_typ
val int_enum: discrete_typ
val char_enum: discrete_typ
val float_enum: dense_typ
val unit_enum: discrete_typ

val get_tname: _ typ -> string

exception Bad_range of string

exception Bad_value of string
    

(* The type of enumerated values. *)
type 'a t

type dense = a_dense t
type discrete = a_discrete t

(* An integer is an enumerated type. 
 * All adaints belongs to the int_enum type. *)
type adaint = discrete

(* Indicates if a value belongs to a given type. *)
val belongs: 'a typ -> 'a t -> bool

(* Indicates if this value is an integer. *)
val is_int: discrete -> bool
val get_int: discrete -> int

val is_float: dense -> bool
val get_float: dense -> float

val get_char: discrete -> char
val get_bool: discrete -> bool

val mkint: int -> discrete
val mkbool: bool -> discrete
val mkchar: char -> discrete
val mkfloat: float -> dense
val unit: discrete
  
(* tostring *)
val e2s: 'a t -> string

(* of_string, may raise Bad_value *)
val ofs: 'a typ -> string -> 'a t

(* First value, as returned by Type'First. *)
val first: 'a typ -> 'a t

(* Last value, as returned by Type'Last. *)
val last: 'a typ -> 'a t

(* Length, as returned by Type'Length *)
val length: discrete_typ -> adaint

(* Returns first, last *)
val range: 'a typ -> 'a t * 'a t

(* Maps index to a value, as returned by Type'Val. Not for dense enumerations. *)
val nth: discrete_typ -> adaint -> discrete

(* Succ, pred *)
val succ: discrete -> discrete
val pred: discrete -> discrete

(* Maps value to its index, as returned by Type'Pos. Not for dense enumerations. 
 * Not sure it works for Integer'Pos or a subtype of Integer.  *)
val index: discrete -> adaint

(* cmp x y > 0 iff x > y 
 * cmp x y = 0 iff x = y *)
val cmp: 'a t -> 'a t -> int

(* Create a subtype *)
val subcreate: name:string -> parent:'a typ -> first:'a t -> last:'a t -> 'a typ
  
(* Lift some operators *)
val apply_int: (int -> int) -> discrete -> discrete
val apply_float: (float -> float) -> dense -> dense

val apply2_int: (int -> int -> int) -> discrete -> discrete -> discrete
val apply2_float: (float -> float -> float) -> dense -> dense -> dense

