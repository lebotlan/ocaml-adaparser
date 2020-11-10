open Ast
open Loc
open Idents
    
val eq_id : loc_ident -> core_expr loc -> bool

(* Qualified *)
val eq_longid: long_ident -> core_expr loc -> bool

val li2expr: long_ident -> expr

val is_null : expr -> bool

val empty_file: string -> file


  
  
