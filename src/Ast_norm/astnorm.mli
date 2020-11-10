open Astlib
open Astreader
open Astmap    
open Loc
open Cost


(* Remove comments *)
val clear: unit tree_mapper

(* Flatten nested sequences. 
 * Unordered sequences become ordered (this is safe). *)
val flatten_seq: unit tree_mapper

(*  Vardefs of the form X : integer := 0   are split into X : integer and X := 0   (we do this for constants too)
 *  Note : this does not preserve the semantics whenever a *type* uses a variable defined before, in the same declaration list.
 *         we accept this however, since the rewritten program happens to be equivalent to a well-formed program, by a simple transformation ;
 *         hence, we consider that the rewritten program "could have easily been well written initially".
 *
 *  For this reason, as well as constants being split too, the resulting program is not guaranteed to compile, even if the original program is compilable.
 *
 *  Run flatten_seq after this.
*)
val expand_var_init: unit short_mapper

(* Normalization, semantics-preserving. See the first part of cost.ml. 
 * Invoke flatten before and after.*)
val norm_keep_semantics: (cost loc list) tree_mapper



(*
 * Puis : regrouper dans des SEQ non ordonnées les blocs qui sont indépendants (réfléchir à l'algo). 
*)

(*
Agglomérer les TxT.Put successifs. Remplacer les put_line par put & EOL 

normaliser les chaînes : remplacer des chaînes par la chaîne représentante de  la classe d'équivalence.

deux temps : collecter les chaînes, puis appliquer...
*)

    

(* Unifier deux AST, avec coût d'unification. *)

