open Astlib
open Ast
open Ast_env
open Use_env
open Astreader.Astmap
open Idents
    
(* Namespace: contains renamings, USE clauses, and local variables. *)
class nmenv: env -> use_env ->
  object ('s)
    method tos: string
    
    method get_defs: ('s * procdef) list
    method get_env: env
    method get_use_env: use_env
      
    method insert_def: 's * procdef -> 's
    method insert_env: loc_ident -> id_typ -> 's
    method insert_use: long_ident -> 's

    method block_exit: 's -> 's
    method merge: (acu1:'s -> 's * (acu2:'s -> 's * 'c)) -> 's * 'c
    method userfun: 's user_fun
  end

class ['a] envmap: 'a user_fun ->
  object
    inherit ['a] tree_mapper
    constraint 'a = #nmenv
  end
  

