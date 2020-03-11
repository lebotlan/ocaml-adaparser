open Nmspace
open Astlib
open Idents
open Ast
    
(*** Finds dependencies between expressions. ***)

type denv

(* Effects of an expression. *)
type effects =
  {
    (* Environment in which this expression has been considered. *)
    denv: denv ;
    expr: expr ;

    (* Local variables, read/written by this expression. *)
    reads: loc_ident list ;
    writes: loc_ident list ;

    (* Which identifiers are bound by this expression for the following expressions
     * (in Ada, probably empty all the time). *)
    defines: loc_ident list ;
    
    (* Side effects, when calling functions or procedures. *)
    side_effects: bool }

val get_effects: denv -> expr -> effects

(*
 * commutable a b  <=>  a ; b is equivalent to b ; a
 * (commutable is symmetric)
 *)
val commutable: effects -> effects -> bool

(* Receives an ordered sequence (containing no subsequence),
 * returns an ordered sequence possibly containing unordered subsequences. *)
val cut_seq: denv -> expr list -> expr list



(* Transforms sequences in this procedure using cut_seq. *)
val unseq_proc: (#nmenv * procdef) -> procdef
