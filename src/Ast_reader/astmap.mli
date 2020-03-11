open Astlib
open Idents
open Adavalues.Adavalue
open Ast
open Parse_errors
open Loc
    
(*** See astmap.ml for all explanations. The comments are there. ***)

type ('v,'a) ret =
  { rval: 'v ;
    acu: 'a }

type ('v, 'a) mapper = 'v -> 'a -> ('v, 'a) ret

type 'a user_fun =
  { block_exit: 'a -> 'a -> 'a ;
    merge_pre: acu0:'a -> 'a ;
    merge_mid: acu0:'a -> acu1:'a -> 'a ;
    merge_end: acu0:'a -> acu1:'a -> acu2:'a -> 'a }

(* This is an example which simply accumulates. *)
val accumulates: 'a user_fun

(* And this one is totally scoped. *)
val scoped: 'a user_fun

val return: 'v -> 'a -> ('v,'a) ret

val (let+): ('a, 'b) ret -> ('a -> 'c) -> ('c, 'b) ret
val (and+): ('a, 'b) ret -> ('b -> ('c, 'd) ret) -> ('a * 'c, 'd) ret
val (let=): ('a, 'b) ret -> ('a -> 'b -> 'c) -> 'c
val (let-): ('a, 'b) ret -> ('a * 'b -> 'b) -> ('a, 'b) ret

val mapacu: ('v,'a) ret -> ('a -> 'b) -> ('v, 'b) ret

type label_namespace = A | S

type arg_kind = Proc_arg of bool | Typedef_arg

type procdecl_kind = Pdk_only_decl | Pdk_funrename | Pdk_procdef

type dkind = Toplevel | In_package | In_declare | In_proc


class ['a] tree_mapper: 'a user_fun ->
  object
    method block : 'a -> ('v, 'a) ret -> ('v, 'a) ret
    
    method adavalue: (expr adavalue, 'a) mapper
    method app: (expr * nexpr list, 'a) mapper
    method arg: arg_kind -> (arg, 'a) mapper
    method argname: arg_kind -> (loc_ident, 'a) mapper
    method attribute: (loc_ident, 'a) mapper
    method comments: (string list, 'a) mapper
    method content: (pl_declarations, 'a) mapper
    method core_expr: pos -> label_namespace -> (core_expr, 'a) mapper
    method core_vardef: (vardef, 'a) mapper
    method declaration: dkind -> (declaration, 'a) mapper
    method declare: (pl_declarations * expr, 'a) mapper
    method declare_expr: (expr, 'a) mapper
    method enumerate_value: (loc_ident, 'a) mapper
    method expr: label_namespace -> (expr, 'a) mapper
    method expr_id: label_namespace -> (loc_ident, 'a) mapper
    method file: (file pv, 'a) mapper
    method for_id: (loc_ident, 'a) mapper
    method fun_id: (long_ident, 'a) mapper
    method fun_rename: (fun_rename, 'a) mapper
    method lbl_id: (loc_ident, 'a) mapper
    method long_id: (long_ident, 'a) mapper
    method ltype: (ltype, 'a) mapper
    method new_id: (long_ident, 'a) mapper
    method nexpr: (nexpr, 'a) mapper
    method pack_id: (loc_ident, 'a) mapper
    method pack_rename: (pack_rename, 'a) mapper
    method packname: (long_ident, 'a) mapper
    method packnew: (packnew, 'a) mapper
    method pl_declarations: dkind -> (pl_declarations, 'a) mapper
    method pnew_id: (loc_ident, 'a) mapper
    method proc_body: (expr, 'a) mapper
    method procdecl: procdecl_kind -> (procdecl, 'a) mapper
    method procdef: (procdef, 'a) mapper
    method procname: procdecl_kind -> (loc_ident, 'a) mapper
    method record_field: (vardef, 'a) mapper
    method select_id: (loc_ident, 'a) mapper
    method subconstraint: (subt_constraint, 'a) mapper
    method subtype: (long_ident, 'a) mapper
    method subtypedef: (subtypedef, 'a) mapper
    method type_expr: (type_expr, 'a) mapper
    method type_id: (loc_ident, 'a) mapper
    method typedef: (typedef, 'a) mapper
    method typename: (long_ident, 'a) mapper
    method use_id: (long_ident, 'a) mapper
    method usetype_id: (long_ident, 'a) mapper
    method var_id: (loc_ident, 'a) mapper
    method vardef: dkind -> (vardef, 'a) mapper
    method when_id: (loc_ident, 'a) mapper
    method whenc: (when_clause, 'a) mapper
    method with_id: (long_ident, 'a) mapper
    method withclause: (withclause, 'a) mapper        
  end


class ['b] short_mapper: 'a tree_mapper -> ('a -> 'b) -> ('b -> 'a) ->
  object
    method file: (file pv, 'b) mapper
    method procdef: (procdef, 'b) mapper
  end
    
