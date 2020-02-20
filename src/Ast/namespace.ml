open Ast
open Idents

type env_rename = pack_rename list

type namespace =
  { renamings: env_rename ;
    use: long_ident list }

let init_namespace =
  { renamings = [] ;
    use = [] }

let rec applyrn_id x = function
  | [] -> [x]
  | pr :: ys ->
    if equal x pr.pack_alias then pr.pack_orig
    else applyrn_id x ys

let applynm_li nmsp = function
  | [] -> []
  | x :: xs -> (applyrn_id x nmsp.renamings) @ xs 

let insert_pr p nm = { nm with renamings = p :: nm.renamings }

let insert_use u nm =
  let u' = applynm_li nm u in
  { nm with use = u' :: nm.use }

let applynm_arg nmsp arg = { arg with argtype = applynm_li nmsp arg.argtype }

let applynm_procdecl nmsp decl =
  { procname = decl.procname ; 
    args = List.map (applynm_arg nmsp) decl.args ;
    rettype = Common.option_map decl.rettype (applynm_li nmsp) }
(*
and applynm_expr nmsp e = match e with
  | Value _ -> e (* This is a builtin function, it is not supposed to be renamed. *)
  | Id li -> Id (applynm_li nmsp li)
  | Tuple nexprs -> Tuple (List.map (applynm_nexpr nmsp) nexprs)
  | Assign (e1, e2) -> Assign (applynm_expr nmsp e1, applynm_expr nmsp e2)
  | App (e1, nexprs) -> App (applynm_expr nmsp e1, List.map (applynm_nexpr nmsp) nexprs)
  | Select (e, li) -> Select (applynm_expr nmsp e, li)
  | Tick (e1, e2) -> Tick (applynm_expr nmsp e1, applynm_expr nmsp e2)
  | If (e1, e2, e3) -> If (applynm_expr nmsp e1, applynm_expr nmsp e2, applynm_expr nmsp e3)
  | While (e1, e2) -> While (applynm_expr nmsp e1, applynm_expr nmsp e2)
  | Exitwhen e -> Exitwhen (applynm_expr nmsp e)
  | For     of [`OF | `IN] * bool * loc_ident * expr * expr
  | Declare of declaration list * expr
  | Case    of expr * (when_clause list)
  | Return  of expr
  | Seq     of expr list
  | New     of long_ident * (expr list)
  | Is_in   of expr * expr
  | Try     of expr * (when_clause list)
  | Unconstrained (* <> *)
  | Interval of expr * expr (* 0..50 *)
  | TickRange of expr * (expr adavalue option) (* e.g. Bla'Range(2) *)
  | Range of expr * expr (* Integer range e *) 
*)

let ident_complies ~ref (nmsp, li) =

  (* Apply namespace (renamings are inlined). *)
  let li = applynm_li nmsp li in

  (* Compare backwards. *)
  let rev_ref = List.rev ref
  and rev_li  = List.rev li in

  (* We remove equal idents. *)
  let rec loop = function
    | l, [] -> Some l (* what remains *)
    | [], _ -> None (* li name is too long *)
    | rx :: rxs, lx :: lxs -> if equal rx lx then loop (rxs,lxs) else None
  in

  match loop (rev_ref, rev_li) with
  | None -> false (* Not equal *)
  | Some [] -> true (* Equal *)
  | Some p ->
    (* Find if p is a prefix in USE clauses. *)
    let p = List.rev p in

    List.exists (long_equal p) nmsp.use
