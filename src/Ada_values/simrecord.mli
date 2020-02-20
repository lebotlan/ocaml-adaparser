(* Simulates a record value. *)
type 'a t

(* Access to a missing field. *)
exception Badfield of string

(* create : list of fields * value 
 * cs: case sensitive *)
val create: cs:bool -> (string * 'a) list -> 'a t

(* Assignment 
 * @raise Badfield *)
val assign: 'a t -> string -> 'a -> unit

(* Read 
 * @raise Badfield *)
val read: 'a t -> string -> 'a


(* Record type *)
type 'a field =
  { fname: string ;
    ftype: 'a }
    
type 'a rtype =
  { tcs: bool ;
    tfields: 'a field list }

val belongs: ('a -> 'b -> bool) -> 'a rtype -> 'b t -> bool

(* Pretty-print *)
val r2s: ?margin:string -> ('a -> string) -> 'a t -> string

val ft2s: ('a -> string) -> 'a field -> string

val rt2s: ('a -> string) -> 'a rtype -> string
