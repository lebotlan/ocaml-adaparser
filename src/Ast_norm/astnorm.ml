open Astlib
open Ast
open Adabuiltins
open Loc
open Cost
open Astreader.Astmap
open Asttool

let null = li2expr (Idents.li_null)

let is_true e = eq_longid Idents.li_true e
let is_false e = eq_longid Idents.li_false e

(* let () = Astprint.verbose := true  *)

(*** Clear : remove comments ***)
let clear =
  object
    inherit [unit] tree_mapper scoped as super

    method! procdef def acu =
      let+ def = super#procdef def acu in
      { def with proc_comments = [] }

    method! declaration kind dl acu =
      let+ decl = super#declaration kind dl acu in
      match decl with
      | Package pc -> Package { pc with package_comments = [] }
      | d -> d

    method! file fpv acu =
      let+ p_file = super#file fpv acu in
      { p_file with pv = { p_file.pv with file_comments = [] } }
    
  end

(*** Flatten SEQ in expressions ***)

let flatten_seq =
  object
    inherit [unit] tree_mapper scoped as super

    method! core_expr pos ln e acu =
      let+ e = super#core_expr pos ln e acu in

      let rec flatten_expr ord acu e = match e.v with
        (* We integrate only sequences of the same type: unordered with unordered, ordered with ordered. *)
        | Seq (flag,el) when flag = ord -> flatten_el ord acu el
        | _ -> e :: acu

      and flatten_el ord acu el = List.fold_left (flatten_expr ord) acu el in
      
      match e with
      | Seq (ord,el) ->
        let flat = List.rev (flatten_el ord [] el) in
        begin match List.filter (fun e -> not (is_null e)) flat with
          | [] -> null.v
          | [e] -> e.v
          | el -> Seq (ord,el)
        end
        
      | x -> x
  end


(*** Expand variable initialisation ***)

type var_inits = vardef list

let take_init vardef = Common.option_map vardef.vinit
    (fun e -> { pos = e.pos ; v = Assign ({ pos = vardef.varname.pos ; v = Id vardef.varname }, e) } )

(* Inserts the given vardefs before e *)
let insert_seq vardefs e =
  let inits = Common.revmapfilter vardefs take_init in
  { pos = e.pos ; v = Seq (true, inits @ [e]) }

(* Inner mapper, that handles an acu of type var_inits *)
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
      let re = o#expr d_ctxt e [] in
      assert (re.acu = []) ;
      return (insert_seq acu re.rval) []

    method! proc_body e acu = o#declare_expr e acu
    
  end

(* Outer mapper, no acu (unit). *)
let expand_var_init = new short_mapper expand_var_map (fun _ -> ()) (fun () -> [])
  

(*** Normalize, keeping semantics. See cost.ml ***)

let expr_not e = App ({ pos = e.pos ; v = Value Builtins.bnot }, [ ([], e) ])

let expr_and e1 e2 = App ({ pos = e1.pos ; v = Value Builtins.band }, [ ([], e1) ; ([], e2) ])

type costs = cost loc list

let norm_keep_semantics =
  object(self)
    inherit [costs] tree_mapper accumulates as super
       
    (* Tautology : X = True instead of X
     * Style_boolean : X = false instead of not X *)
    method! core_expr pos ln e acu = self#rewrite_expr pos ln (super#core_expr pos ln e acu)

    (* Not sure if we should make recursive calls after each rewriting. *)
    method private rewrite_expr pos _ln re =

      let acu = re.acu in
      
      match re.rval with

      (* A tuple with one non-labeled element (e) => e *)
      | Tuple [ ([], e) ] -> return e.v acu
      
      | App (ee, neel) ->
        begin match ee.v, neel with

          (* x >  y  =>  x <  y 
           * x >= y  =>  x <= y *)
          | Value op, [ ([], e1) ; ([], e2) ] when (op == Builtins.geq || op == Builtins.gt) ->
            let newop = if op == Builtins.geq then Builtins.leq else Builtins.lt in
            (* self#rewrite_expr pos ln *) { rval = App ( { pos = ee.pos ; v = Value newop } , [ ([], e2) ; ([], e1) ]) ; acu }

          (* not (x < y)  not (x <= y)   =>   y <= x    y < x *)
          | Value op, [ ([], { v = App ( { v = Value op2 ; pos = pos2 } ,
                                         [ ([], e1) ; ([], e2) ] ) ;
                               pos = _ } ) ]
            when op == Builtins.bnot && (op2 == Builtins.leq || op2 == Builtins.lt) ->

            let op3 = if op2 == Builtins.leq then Builtins.lt else Builtins.leq in            
            { rval = App ( { pos = pos2 ; v = Value op3 } , [ ([], e2) ; ([], e1) ]) ; acu }          
            
          | Value op, [ ([], e1) ; ([], e2) ] when op == Builtins.equal ->

            (* True = e2 => e2 *)
            if is_true e1 then return e2.v (mkloc (e1.pos.start, ee.pos.endp) Tautology :: acu)

            (* e1 = True => e1 *)
            else if is_true e2 then return e1.v (mkloc (ee.pos.start, e2.pos.endp) Tautology :: acu)

            (* False = e2 => not e2 *)
            else if is_false e1 then return (expr_not e2) (mkloc (e1.pos.start, ee.pos.endp) Style_boolean :: acu)

            (* e1 = False => not e1 *)
            else if is_false e2 then return (expr_not e1) (mkloc (ee.pos.start, e2.pos.endp) Style_boolean :: acu)
          
            else re
                
          | _ -> re
        end

      | Assign ( { v = Id id1 ; _ } , e2) ->

        (* null block not written null, usually X := X *)
        if eq_id id1 e2 then return null.v ( { pos ; v = Null } :: acu)
        else re

      | If (_, e2, e3) when Astcmp.cmp_expr e2 e3 = 0 ->
        (* then A else A *)  (*** FIXME TODO : check that the condition has no side-effect !!! *)
        return e2.v ( { pos ; v = Useless_if } :: acu) 

      | If (c1, { v = If (c2, e1, e2) ; pos }, e3) when Astcmp.cmp_expr e2 e3 = 0 ->
        (* IF compaction: if cond1 then if cond2 then A (no elses, or identical elses) *)
        return (If ({ v = expr_and c1 c2 ; pos = e1.pos }, e1, e2)) ( { pos ; v = If_compaction } :: acu) 
    
      | _ -> re

  end
