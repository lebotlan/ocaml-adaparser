open Astlib
open Idents
open Adavalues.Adavalue
open Ast
open Parse_errors
    
(*** See astmap.ml for all explanations ***)

type ('v,'a) ret =
  { rval: 'v ;
    acu: 'a }

type ('v, 'a) mapper = 'v -> 'a -> ('v, 'a) ret

type 'a user_fun =
  { block_exit: 'a -> 'a -> 'a ;
    merge: 'c . acu0:'a -> (acu1:'a -> 'a * (acu2:'a -> 'a * 'c)) -> ('a * 'c) }

(* This is an example which simply accumulates. *)
val accumulates: 'a user_fun

val return: 'v -> 'a -> ('v,'a) ret

val (let+): ('a, 'b) ret -> ('a -> 'c) -> ('c, 'b) ret
val (and+): ('a, 'b) ret -> ('b -> ('c, 'd) ret) -> ('a * 'c, 'd) ret
val (let=): ('a, 'b) ret -> ('a -> 'b -> 'c) -> 'c

val mapacu: ('v,'a) ret -> ('a -> 'b) -> ('v, 'b) ret

type label_namespace = A | S

class ['a] tree_mapper: 'a user_fun ->
  object
    method adavalue: (expr adavalue, 'a) mapper
    method arg: (arg, 'a) mapper
    method argname: (loc_ident, 'a) mapper
    method argtype: (long_ident, 'a) mapper
    method comments: (string list, 'a) mapper
    method content: (pv_declaration list, 'a) mapper
    method declaration: (declaration, 'a) mapper
    method expr: label_namespace -> (expr, 'a) mapper
    method expr_id: label_namespace -> (loc_ident, 'a) mapper
    method file: (file pv, 'a) mapper
    method for_id: (loc_ident, 'a) mapper
    method fun_id: (long_ident, 'a) mapper
    method lbl_id: (loc_ident, 'a) mapper
    method long_id: (long_ident, 'a) mapper
    method ltype: (ltype, 'a) mapper
    method new_id: (long_ident, 'a) mapper
    method nexpr: (nexpr, 'a) mapper
    method pack_id: (loc_ident, 'a) mapper
    method pack_rename: (pack_rename, 'a) mapper
    method packname: (long_ident, 'a) mapper
    method pnew_id: (loc_ident, 'a) mapper
    method procdecl: (procdecl, 'a) mapper
    method procdef: (procdef, 'a) mapper
    method procname: (loc_ident, 'a) mapper
    method pv_declaration: (declaration list pv, 'a) mapper
    method rettype: (long_ident, 'a) mapper
    method select_id: (loc_ident, 'a) mapper
    method subconstraint: (subt_constraint, 'a) mapper
    method subtype: (long_ident, 'a) mapper
    method type_expr: (type_expr, 'a) mapper
    method type_id: (loc_ident, 'a) mapper
    method use_id: (long_ident, 'a) mapper
    method usetype_id: (long_ident, 'a) mapper
    method var_id: (loc_ident, 'a) mapper
    method when_id: (loc_ident, 'a) mapper
    method whenc: (when_clause, 'a) mapper
    method with_id: (long_ident, 'a) mapper
    method withclause: (withclause, 'a) mapper        
  end

    
