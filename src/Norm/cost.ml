
(* Cost of rewriting rules. *)

type cost =
  (* X = true instead of X *)
  | Tautology

  (* X = false instead of not X *)
  | Style_boolean

  (* null block, usually X := X *)
  | Null

  (* IF compaction: if cond1 then if cond2 then A (no elses, or identical elses) *)
  | If_compaction
    


(* Remplacer une variable constane par sa valeur.  (attention, l'expression ne doit pas changer de valeur non plus)
 * Exemple : dernier : Integer := mat'last(1)  *)


