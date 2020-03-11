open Loc
    
(* Shared strings, see Shared_vals *)

type s

val norm: string -> s

type loc_ident = s loc
type long_ident = loc_ident list

(* These are used to pretty-print, but ALSO to compare arguments/function names. Do not add extra symbols. *)
val i2s: s -> string
val l2s: loc_ident -> string
val li2s: long_ident -> string

val empty_ident: loc_ident
val empty_long_ident: long_ident

val equal: loc_ident -> loc_ident -> bool
val is_empty: loc_ident -> bool

val s_equal: s -> s -> bool

val long_equal: long_ident -> long_ident -> bool

val get_li_pos: long_ident -> pos

val reloc: pos -> long_ident -> long_ident

(* Predefined identifiers *)

(* "access", "all", "others", "exception" *)
val access: s
val all: s
val others: s
val i_exception: s
val i_exit: s

val i_true: s
  
(* Maps a long_ident package name to a file name. *)
val pack2file: long_ident -> string
  
