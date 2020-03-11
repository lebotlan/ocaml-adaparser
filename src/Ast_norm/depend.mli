open Nm_qualify
open Astlib
open Idents
open Ast
    
(*** Deal with dependencies between expressions. ***)

(* Effects of an expression. *)
type effects =
  {
    (* Environment in which this expression has been considered. *)
    nms: qenv ;
    expr: expr ;

    (* Local variables, read/written by this expression. *)
    read: loc_ident list ;
    writes: loc_ident list ;

    (* Which identifiers are bound by this expression (probably empty all the time). *)
    defines: loc_ident list ;
    
    (* Side effects, when calling functions or procedures. *)
    side_effects: bool }

val get_effects: qenv -> expr -> effects

(* Compares two expression e1 and e2, effect-wise.
 * commutable a b  <=>  a ; b is equivalent to b ; a
 * (commutable is symmetric)
 *)
val commutable: effects -> effects -> bool

(* Receives an ordered sequence (with no subsequence),
 * returns an ordered sequence possibly containing unordered subsequences. *)
val cut_seq: qenv -> expr list -> expr list
