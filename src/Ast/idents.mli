open Loc
    
(* Shared strings, see Shared_vals *)

type s

val norm: string -> s
val prefix: string -> s -> s

type loc_ident = s loc
type long_ident = loc_ident list

(* These are used to pretty-print, but ALSO to compare arguments/function names. Do not add extra symbols. *)
val i2s: s -> string
val l2s: loc_ident -> string
val li2s: long_ident -> string

val li_cmp: long_ident -> long_ident -> int

val empty_ident: loc_ident
val empty_long_ident: long_ident

val equal: loc_ident -> loc_ident -> bool
val is_empty: loc_ident -> bool

val s_equal: s -> s -> bool

val long_equal: long_ident -> long_ident -> bool

(* Do not use Hashtbl.mem, List.assoc with idents ! *)
val i_assoc_opt: loc_ident -> (loc_ident * 'a) list -> 'a option
val li_assoc_opt: long_ident -> (long_ident * 'a) list -> 'a option

val hashli: long_ident -> int
module Hashli : Hashtbl.S  with type key = long_ident

val get_li_pos: long_ident -> pos

val reloc: pos -> long_ident -> long_ident

(* Convenience : predefined identifiers 
 * (they could be defined anywhere, these are not the builtin definitions) 
 *)

val i_exception: s
val i_access: s
val i_all: s
val i_exit: s
val i_stdlib: s

val l_true: loc_ident
val l_false: loc_ident
val l_null: loc_ident
val l_stdlib: loc_ident

val li_stdlib: long_ident
val li_null: long_ident (* Stdlib.null *)
val li_true: long_ident
val li_false: long_ident
  
(* Maps a long_ident package name to file names, without the extension. *)
val pack2filename: long_ident -> string
  
