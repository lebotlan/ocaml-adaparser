open Astlib
open Astreader
open Astmap
open Ast
open Adabuiltins
open Loc
open Cost
    

let is_id id = function
  | Id x -> Idents.equal x id
  | _ -> false

(*** Expand variable initialisation ***)

type var_inits = vardef list

let take_init vardef = Common.option_map vardef.vinit (fun e -> Assign (Id vardef.varname,e))

let insert_seq vardefs e =
  let inits = Common.revmapfilter vardefs take_init in
  Seq (true, inits @ [e])

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

let id_null = Ast_env.id_null

let flatten_seq =
  object
    inherit [unit] tree_mapper scoped as super

    method! expr ln e acu =
      let+ e = super#expr ln e acu in

      let rec flatten_expr acu = function
        | Seq (_,el) -> flatten_el acu el
        | x -> x :: acu

      and flatten_el acu el = List.fold_left flatten_expr acu el in
      
      match e with
      | Seq (_,el) ->
        let flat = List.rev (flatten_el [] el) in
        begin match List.filter (fun e -> not (is_id id_null e)) flat with
          | [] -> Id id_null
          | [e] -> e
          | el -> Seq (true,el)
        end
        
      | x -> x
  end

(*** Normalize, keeping semantics. See norm.ml ***)

let id_true = Ast_env.id_true
let id_false = Ast_env.id_false

let expr_not e = App (Value Builtins.bnot, [ ([], e) ])

type costs = cost loc list

let norm_keep_semantics =
  object(o)
    inherit [costs] tree_mapper accumulates as super
       
    (* Tautology : X = True instead of X *)
    method! expr ln e acu =
      match e with
      | App (e, nel) ->
        let { rval = (ee, neel) ; acu } = o#app (e, nel) acu in
        
        begin match ee, neel with
          | Value op, [ ([], e1) ; ([], e2) ] when op == Builtins.equal ->

            (* True = e2 => e2 *)
            if is_id id_true e1 then return e2 acu (* return e2 (mkloc (e1.pos.start, e1.pos.endp) Tautology :: acu) *)

            (* e1 = True => e1 *)
            else if is_id id_true e2 then return e1 acu

            (* False = e2 => not e2 *)
            else if is_id id_false e1 then return (expr_not e2) acu

            (* e1 = False => not e1 *)
            else if is_id id_false e2 then return (expr_not e1) acu
          
            else return (App (ee, neel)) acu
                
          | _ -> return (App (ee, neel)) acu
        end
         
      | _ -> super#expr ln e acu

  end
      




