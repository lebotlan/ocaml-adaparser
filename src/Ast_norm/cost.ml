
(* Cost of rewriting rules. *)

type cost =

  (*** Preserved semantics ***)
  
  (* X = true instead of X *)
  | Tautology

  (* X = false instead of not X *)
  | Style_boolean

  (* null block, usually X := X *)
  | Null

  (* IF compaction: if cond1 then if cond2 then A (no elses, or identical elses) *)
  | If_compaction


  (*** Modified semantics ***)

  (* < instead of <=, > instead of >= *)
  | Strict_comparison

  (* not X instead of X, < instead of >, ... *)
  | Inverted_condition

  (* OR instead of AND, ... *)
  | Inverted_boolean_operator

  (* Replace a constant by another one, e.g. Integer'Last by 99999. 
   * Beware, constant substitution can be harmful depending on the context (e.g. replacing 0 by 1, ...) *)
(* how to decide when it is serious or not ??? *)

(* Remplacer une variable constante par sa valeur.  (attention, l'expression ne doit pas changer de valeur non plus)
 * Exemple : dernier : Integer := mat'last(1)  *)


