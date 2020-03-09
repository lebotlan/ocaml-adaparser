open Astlib
open Astreader
open Astmap
open Ast
open Adabuiltins
open Adavalues
open Loc
open Cost
    
let id_null = Ast_env.id_null

let is_id id e = match e.v with
  | Id x -> Idents.equal x id
  | _ -> false

let rec is_null e =
  is_id id_null e
  ||
  match e.v with
  | Seq (_,el) -> List.for_all is_null el
  | Value w -> Adavalue.(0 = cmp unit w)
  | _ -> false

let is_null e =
  Printf.printf "is_null? on %s\n%!" (Astprint.expr2s ~margin:"" e) ;
  let res = is_null e in
  Printf.printf "Result is %b\n%!" res ;
  res

(*** Expand variable initialisation ***)

type var_inits = vardef list

let take_init vardef = Common.option_map vardef.vinit
    (fun e -> { pos = e.pos ; v = Assign ({ pos = vardef.varname.pos ; v = Id vardef.varname}, e) } )

let insert_seq vardefs e =
  let inits = Common.revmapfilter vardefs take_init in
  { pos = e.pos ; v = Seq (true, inits @ [e]) }

let expand_var_map =
  object(o)
    inherit [var_inits] tree_mapper scoped as super

    method! vardef kind v a =
      let res = super#vardef kind v a in
      match kind with
      | Toplevel | In_package -> res
      | In_declare | In_proc ->        
        let vdef = { res.rval with vinit = None } in
        return vdef (res.rval :: res.acu)

    method! declare_expr e acu =
      let re = o#expr S e [] in
      assert (re.acu = []) ;
      return (insert_seq acu re.rval) []

    method! proc_body e acu = o#declare_expr e acu
    
  end

let expand_var_init = new short_mapper expand_var_map (fun _ -> ()) (fun () -> [])
  
(*** Flatten SEQ in expressions ***)

let flatten_seq =
  object
    inherit [unit] tree_mapper scoped as super

    method! core_expr pos ln e acu =
      let+ e = super#core_expr pos ln e acu in

      let rec flatten_expr acu e = match e.v with
        | Seq (_,el) -> flatten_el acu el
        | _ -> e :: acu

      and flatten_el acu el = List.fold_left flatten_expr acu el in
      
      match e with
      | Seq (_,el) ->
        let flat = List.rev (flatten_el [] el) in
        begin match List.filter (fun e -> not (is_id id_null e)) flat with
          | [] -> Id id_null
          | [e] -> e.v
          | el -> Seq (true,el)
        end
        
      | x -> x
  end

(*** Normalize, keeping semantics. See norm.ml ***)

let id_true = Ast_env.id_true
let id_false = Ast_env.id_false

let expr_not e = App ({ pos = e.pos ; v = Value Builtins.bnot }, [ ([], e) ])

type costs = cost loc list

let norm_keep_semantics =
  object(_o)
    inherit [costs] tree_mapper accumulates as super
       
    (* Tautology : X = True instead of X *)
    method! core_expr pos ln e acu =

      let re = super#core_expr pos ln e acu in
      let acu = re.acu in
      
      match re.rval with
      | App (ee, neel) ->
        begin match ee.v, neel with
          | Value op, [ ([], e1) ; ([], e2) ] when op == Builtins.equal ->

            (* True = e2 => e2 *)
            if is_id id_true e1 then return e2.v (mkloc (e1.pos.start, ee.pos.endp) Tautology :: acu)

            (* e1 = True => e1 *)
            else if is_id id_true e2 then return e1.v (mkloc (ee.pos.start, e2.pos.endp) Tautology :: acu)

            (* False = e2 => not e2 *)
            else if is_id id_false e1 then return (expr_not e2) (mkloc (e1.pos.start, ee.pos.endp) Style_boolean :: acu)

            (* e1 = False => not e1 *)
            else if is_id id_false e2 then return (expr_not e1) (mkloc (ee.pos.start, e2.pos.endp) Style_boolean :: acu)
          
            else re
                
          | _ -> re
        end

      | Assign ( { v = Id id1 ; _ } , e2) ->
        if is_id id1 e2 then return (Id id_null) ( { pos ; v = Null } :: acu)
        else re

      | If (_, e2, e3) ->
        if is_null e2 && is_null e3 then return (Id id_null) ( { pos ; v = Useless_if } :: acu)
        else re
        
      | _ -> re

  end
      




