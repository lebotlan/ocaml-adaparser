open Astlib
open Namespace

(*** Check if an Adasig complies with a procdecl ***)

type cmp_result =
  (* Not the expected function. *)
  | Not_a_chance

  (* Expected function, but something is terribly wrong. 
   * We provide an error message. *)
  | Wrong of string

  (* OK. If the message is empty, this is perfect.
   * Otherwise, there is a minor problem that could be fixed (e.g. typo on an argument name, ...) *)
  | OK of string

(* complies what-we-have what-we-want *)
val complies: (namespace * Ast.procdecl) -> Adasig.proc_descr -> cmp_result

val result2s: cmp_result -> string
  
