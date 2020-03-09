open Astlib
open Astreader
open Astmap    
open Loc
open Cost
    
(*  Vardefs of the form X : integer := 0   are split into X : integer and X := 0   (constants are concerned too)
 *  Note : this does not preserve the semantics whenever a type uses a variable defined before, in the same declaration list.
 *         we accept this however, since the rewritten program happens to be equivalent to a well-formed program, by a simple transformation ;
 *         hence, we consider that the rewritten program "could have easily been well written initially".
 *
 *  For this reason, as well as constant splitting, the resulting program is not guaranteed to compile, even if the original program is compilable.
 *
 *  Run flatten_seq after this.
*)
val expand_var_init: unit short_mapper

(* Normalization, semantics-preserving. See the first part of cost.ml. *)
val norm_keep_semantics: (cost loc list) tree_mapper

(* Flatten nested sequences. 
 * Unordered sequences become ordered (this is safe). *)
val flatten_seq: unit tree_mapper



(*
 * Puis : regrouper dans des SEQ non ordonnées les blocs qui sont indépendants (réfléchir à l'algo). 
*)

(*
normaliser les chaînes : remplacer des chaînes par la chaîne représentante de  la classe d'équivalence.

deux temps : collecter les chaînes, puis appliquer...
*)

    

(* Unifier deux AST, avec coût d'unification. *)

