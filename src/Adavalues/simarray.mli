
(* Simulates a n-dimensional array value. *)
type 'a t

(* Domain of one dimension (e.g. domain of rows or domain of columns). *)
type dom =
  { lo: int ;
    hi: int }

type coords = int list

(* create: doms   for a n dimensional array => doms is a list of n elements *)
val create: dom list -> 'a -> 'a t

(* Bad_dimensions (expected, received) *)
exception Bad_dimensions of int * int
                            
exception Out_of_range of coords

val put : 'a t -> coords -> 'a -> unit
val get : 'a t -> coords -> 'a

val get_doms: 'a t -> dom list

(* Get slice *)
val slice : 'a t -> dom list -> 'a t

(* Array type *)
type 'a atype =
  { (* Domains. None means unconstrained. *)
    tdom: dom option list ;

    acontent: 'a }

val a2s: ?margin:string -> ('a -> string) -> 'a t -> string

val dom2s: dom -> string

val coords2s: coords -> string

val at2s: ('a -> string) -> 'a atype -> string

(* belongs value_belongs atyp ar *)
val belongs: ('a -> 'b -> bool) -> 'a atype -> 'b t -> bool
