open Ast
open Parse_errors

let all_errors_withclause _ = punit

let rec all_errors_decl : 'a . 'a declaration -> _ = function
  | Procdef proc -> all_errors_procdef proc
  | Rename _ -> punit
  | Packnew _ -> punit
  | Funrename _ -> punit
  | Typedef _ -> punit
  | Subtype _ -> punit
  | Vardef _ -> punit
  | Withclause w -> all_errors_withclause w

and all_errors_nexpr (_, e) = all_errors_expr e

and all_errors_when = function
  | Others e -> all_errors_expr e
  | Match (el, e) -> unitjoin all_errors_expr (e :: el)

and all_errors_nexprlist l = unitjoin all_errors_nexpr l

and all_errors_expr = function
  | Value _ -> punit
  | Id _ -> punit
  | Tuple args -> all_errors_nexprlist args
  | Assign (e1, e2) -> all_errors_expr e1 >>> all_errors_expr e2
  | App (e, args) -> all_errors_expr e >>> all_errors_nexprlist args
  | Select (e, _) -> all_errors_expr e
  | Tick (e, _) -> all_errors_expr e
  | If (e1, e2, e3) -> all_errors_expr e1 >>> all_errors_expr e2 >>> all_errors_expr e3
  | While (e1, e2) -> all_errors_expr e1 >>> all_errors_expr e2
  | Exitwhen e -> all_errors_expr e
  | For (_, e1, e2) -> all_errors_expr e1 >>> all_errors_expr e2
  | Declare (decls, e) -> all_errors_declarations decls >>> all_errors_expr e
  | Return e -> all_errors_expr e
  | Seq el -> unitjoin all_errors_expr el
  | New (_, l) -> unitjoin all_errors_expr l

  | Interval (e1, e2)
  | Range (e1, e2)
  | Is_in (e1, e2) -> all_errors_expr e1 >>> all_errors_expr e2
  | Try (e, whens)
  | Case (e, whens) -> all_errors_expr e >>> unitjoin all_errors_when whens

  | Unconstrained -> punit
  | TickRange (e, _) -> all_errors_expr e
  


and all_errors_procdef : 'a . 'a procdef -> _ = fun proc ->
  proc.sub_errors >>>
  all_errors_declarations proc.declarations >>>
  all_errors_expr proc.body
  
and all_errors_declarations : 'a . 'a declaration list -> _ = fun l -> unitjoin all_errors_decl l

let all_errors_package : 'a . 'a package -> _ = fun pack ->
  all_errors_declarations pack.package_declarations >>>
  match pack.package_init with
  | None -> punit
  | Some e -> all_errors_expr e

let all_errors_cunit = function
  | Program proc -> all_errors_procdef proc
  | Package_Sig pack -> all_errors_package pack
  | Package_Body pack -> all_errors_package pack
  | No_cu -> punit


let all_errors_ast ast =
  unitjoin all_errors_withclause ast.clauses >>>
  all_errors_cunit ast.c_unit

let all_errors fast = fast.ast >>= all_errors_ast







