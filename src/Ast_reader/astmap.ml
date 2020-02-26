(*** Generic MAP & FOLD on the AST ***)

(*
 * Accumulators may contain different kinds of data:
 *    - persistent data, e.g. counting the total number of ADDitions in the whole program.
 *                       such data is never reset, always propagated as such.
 *
 *    - scoped data, e.g. a list of the variables currently in scope
 *                  such data is not propagated outside its current block.
 * 
 *    - control-flow sensitive data, e.g. a list of variable currently in scope that may have been (resp. were certainly) modified.
 *                  such data depends on the control flow. It must be merged at junction points (IF, WHILE).
 *
 * The user is expected to redefine two methods in order to define how acu data is propagated:
 *      #block_exit  : what happens when exiting the current block (a block is e.g. a then part or a else part in C or Java: one may define local variables there. In Ada blocks are more explicit, though.)
 *      #merge       : what happens when two control flows merge.
 *
 *)
open Astlib
open Parse_errors
open Ast

(* Type returned by all mapping methods: a value and a new acu. *)
type ('v,'a) ret =
  { (* rval: returned value *)
    rval: 'v ;
    acu: 'a }

(* A mapper receives the current subtree 'v, the current acu.
 * It returns a mapped subtree and a new acu. *)
type ('v,'a) mapper = 'v -> 'a -> ('v,'a) ret

(* Type of block_exit and merge functions. 
 * 'a is the acu type.
 *
 * The merge function is a bit elaborate.
 *   - A naive merge function would be of type 'a -> 'a -> 'a  (left-acu -> right-acu -> final-acu)
 *
 *   - A better merge function would also receive the initial acu (acu before left and right branches), 
 *     in order to ease the finding of differences between init/left/right: init:'a -> left:'a -> right:'a -> 'a
 *
 *   - Better: we allow the merge function to modify the acu which is passed to the left & right branches.
 *     Hence, it can reset some part of the acu before passing it to one, then to the other branch.
 *
 *     e.g.  acu1 -> left-branch -> acu1'    then  acu1' is modified to acu2 (e.g. something is reset, some flag is set)
 *           acu2 -> right-branch -> acu2'
 *           then acu0, acu1', acu2' are merged into some final acu.
 *
 *   - Merge must have a polymorphic type, which guarantees that the left & right branches were actually invoked.
 *
 *  (Note that both left & right functions called by merge actually build a new acu _and_ a new value.
 *   The new value is used internally to built the subtree, but is eventually hidden from the merge function.
 *   Hence, the real control flow is a bit intricate, but hopefully nicely hidden from the user).
 *
 * *)
type 'a user_fun =
  { (* It receives the input acu (when entering the block) and output acu (once the block is finished). 
     * It must return the final acu. *)
    block_exit: 'a -> 'a -> 'a ;

    (* The type 'c is only a witness that merge actually called both functions. 
     * acu0 is the initial acu (before branches)
     * acu1 is the acu passed to the left branch.
     * acu2 is the acu passed to the right branch. 
     *
     * Expected invariants when merge is "called" with two functions f and g:
     *      - symmetric: merge acu f g should be equivalent merge acu g f
     *      - associative: merge acu (acu -> merge acu (f,g)) h  should be equivalent to merge acu f (acu -> merge acu (g,h))
     *
     * merge is expected to be somewhat associative (used to merge n branches in CASE statements, for instance). *)
    merge: 'c . acu0:'a -> (acu1:'a -> 'a * (acu2:'a -> 'a * 'c)) -> ('a * 'c)
  }

(*
   Examples: 
       (* Accumulates *)
       let block_exit _inacu outacu = outacu

       (* Accumulates *)
       let merge ~acu0 f =
          let (acu2, g) = f ~acu1:acu0 in
          g ~acu2

       (* Reset (e.g. list of scope variables) *)
       let block_exit inacu _outacu = inacu
*)

let accumulates =
  let merge ~acu0 f =
    let (acu2, g) = f ~acu1:acu0 in
    g ~acu2

  and block_exit _inacu outacu = outacu in

  { block_exit ; merge }
  
(* Monadic helper functions *)
let return v acu = { rval = v ; acu }

let mapacu rv f = { rval = rv.rval ; acu = f rv.acu }

(* k builds an unboxed value. *)
let (let+) r k = { r with rval = k r.rval }
let (and+) r g = let+ x = g r.acu in (r.rval,x)

(* k builds a boxed value & expects an acu. *)
let (let=) r k = k r.rval r.acu

let (>>++) ret el =
  let+ tail = ret and+ x = el in
  x :: tail

(* list_map with ret acu. 
 * acu are simply chained
 * (See also mergelist where acus are merged.) *)
let list f l acu =
  let init = return [] acu in
  let+ l = (List.fold_left (fun r x -> r >>++ f x) init l) in
  List.rev l

(* option_map *)
let option f x acu =
  match x with
  | None -> return None acu
  | Some y -> let+ k = f y acu in Some k


(*** Mapper classes ***)

(* Note:
 *   nexpr are labeled expressions, like this:  label => expr
 *   They are used for function application, building records, building arrays.
 *   When building arrays, a label may be a union of labels, ranges, constants, ...
 *   A label may belong to different namespaces (function arguments, record types, constants).
 *
 *   Beware   X := (Y => 10) ;    Y may be a record field, or a constant when building an array. 
 *  
 *   When exploring an expression, we have a flag indicating in which label namespace we expect to be.
 *
*)
type label_namespace =
  (* Function label 
   * A = Argument *)
  | A

  (* Everything else (standard expression) 
   * S = Standard *)
  | S

let find_nexpr_kind = function
  (* Empty nexpr, we don't care. *)
  | [] -> S

  | [Value _] -> S    
  | [Id _] -> A (* Probably an argument name. Can still be a constant, though. *)
  | [_] -> S

  (* Multiple labels: it is not an argument *)
  | _ -> S


(* block_exit receives the acu of the currently finished block and the initial value of the acu when entering this block. 
   merge receives two acus corresponding to two merging control-flows.
*)
class ['a] tree_mapper user_fun =

  (* Merges two branches. *)

  (* Strange bug: if let++ is defined here, compilation fails with some error. 
   * Instead, we define a nice-looking letpp function, and we have to define let++ each time we need it. *)
  let letpp (f,g) k acu =

    let merge_kont1 ~acu1 =
      let rx = f acu1 in

      let merge_kont2 ~acu2 =
        let ry = g acu2 in
        (ry.acu, (rx.rval, ry.rval))
      in

      (rx.acu, merge_kont2)
    in
    
    let (final_acu, (x,y)) = user_fun.merge ~acu0:acu merge_kont1 in
    return (k (x, y)) final_acu
  in

  (* Applies letpp iteratively on a list. *)
  let mergelist mapf l acu =
    let (let++) u v w = letpp u v w in
    
    let rec loop l acu0 = match l with
      | [] -> return [] acu0
      | [x] -> let+ el = mapf x acu0 in [el]
      | x :: xs ->
        acu0 |>
        let++ (y, ys) = (mapf x, loop xs) in
        y :: ys
    in
    
    loop l acu
  in

  let block: 'v . 'a -> ('v,'a) ret -> ('v,'a) ret = fun inacu ret -> return ret.rval (user_fun.block_exit inacu ret.acu) in

  let mkblock f = fun acu -> block acu (f acu) in

  object(o)

    (* Possible optimization: the tree is duplicated here, even if the mapping is the identity.
     *    => one should check, at each node, if new childs == old childs, then return old subtree instead of a copy. *)
    
    (* ln  means label namespace *)

    method expr ln e acu =
      let (let++) u v w = letpp u v w in
      
      match e with
      | Value v -> let+ v = o#adavalue v acu in Value v

      | Id li -> let+ li = o#expr_id ln li acu in Id li

      | Select (e, id) ->
        let+ ee = o#expr S e acu
        and+ iid = o#select_id id in
        Select (ee, iid)

      | For (oi,rev,id,e1,e2) ->
        (* A FOR defines a block. *)
        block acu
          (let+ ee1 = o#expr S e1 acu                      
           (* The for identifier scope is e2 only, hence it comes second. *)
           and+ iid = o#for_id id 
           and+ ee2 = o#expr S e2 in

           For (oi,rev,iid,ee1,ee2))    

      | New (li, el) ->
        let+ lid = o#new_id li acu 
        and+ l = list (o#expr S) el in
        New (lid, l)

      | If (e1, e2, e3) ->        
        let= ee1 = o#expr S e1 acu in

        (* let++  merges the left and right branches 
         * Also, e2 and e3 are considered as blocks. *)
        let++ (ee2, ee3) = (mkblock (o#expr S e2),
                            mkblock (o#expr S e3))
        in
        If (ee1, ee2, ee3)

      | While (e1, e2) ->
        let= ee1 = o#expr S e1 acu in

        (* e2 is a block *)
        let++ (ee2, ()) = (mkblock (o#expr S e2), return ()) in
        While (ee1, ee2)

      | Assign (e1, e2) ->
        let+ ee1 = o#expr S e1 acu 
        and+ ee2 = o#expr S e2 in
        Assign (ee1, ee2)

      | Tick (e1, e2) ->
        let+ ee1 = o#expr S e1 acu
        and+ ee2 = o#expr S e2 in
        Tick (ee1, ee2)

      | Is_in (e1, e2) ->
        let+ ee1 = o#expr S e1 acu
        and+ ee2 = o#expr S e2 in
        Is_in (ee1, ee2)

      | Seq el -> let+ l = list (o#expr S) el acu in Seq l

      | Tuple nel -> let+ l = list o#nexpr nel acu in Tuple l

      | App (e, nel) ->
        let+ ee = o#expr S e acu 
        and+ l = list o#nexpr nel in
        App (ee, l)

      | Exitwhen e -> let+ ee = o#expr S e acu in Exitwhen ee

      | Return e -> let+ ee = o#expr S e acu in Return ee

      | Declare (dl, e) ->
        block acu
          (let+ ll = list o#declaration dl acu
           and+ ee = o#expr S e in
           Declare (ll, ee))

      | Case (e, ws) ->
        let+ ee = o#expr S e acu
        and+ ww = mergelist o#whenc ws in
        Case (ee, ww)

      | Try (e, ws) ->
        let+ ee = o#expr S e acu 
        and+ ww = mergelist o#whenc ws in
        Try (ee, ww)

      | Unconstrained -> return Unconstrained acu

      | Interval (e1, e2) ->
        let+ ee1 = o#expr S e1 acu 
        and+ ee2 = o#expr S e2 in
        Interval (ee1, ee2)

      | Range (e1, e2) ->
        let+ ee1 = o#expr S e1 acu 
        and+ ee2 = o#expr S e2 in
        Range (ee1, ee2)

      | TickRange (e, avo) ->
        let+ ee = o#expr S e acu 
        and+ vvo = option o#adavalue avo in
        TickRange (ee, vvo)

    method adavalue v acu = return v acu

    method whenc (Match (ido, el, e)) acu =
      let+ ido2 = option o#when_id ido acu
      and+ l = list (o#expr S) el 
      and+ ee = mkblock (o#expr S e) in
      Match (ido2, l, ee)

    method nexpr (lbl, e) acu =
      let k = find_nexpr_kind lbl in      
      let+ a = list (o#expr k) lbl acu
      and+ b = o#expr S e in
      (a,b)
    
    (* Id expr *)
    method expr_id _ln li acu = return li acu

    method lbl_id id acu = return id acu

    method comments c acu = return c acu
    
    (* pack rename, pack new, ltype *)
    method long_id li acu = return li acu

    (* Function's return type *)
    method rettype = o#long_id
    method argtype = o#long_id

    (* Subtype t is xxx (this is xxx): *)
    method subtype = o#long_id
    method fun_id = o#long_id    
    method new_id = o#long_id
    method with_id = o#long_id
    method use_id = o#long_id
    method usetype_id = o#long_id
      
    method select_id id acu = return id acu

    (* Binders *)
    method argname id acu = return id acu
    method var_id id acu = return id acu
    method for_id id acu = return id acu
    method when_id id acu = return id acu

    (* Package rename *)
    method pack_id id acu = return id acu
        
    method pnew_id id acu = return id acu
    method packname id acu = return id acu
    method procname id acu = return id acu
    method type_id id acu = return id acu    
       
    (*** Declarations ***)

    (* pv_declaration = declaration list pv *)
    method pv_declaration dlpv acu =
      let+ dl = list o#declaration dlpv.pv acu in
      { pv = dl ; errors = dlpv.errors }
    
    method declaration dl acu = match dl with      
      | Withclause wc -> let+ w = o#withclause wc acu in Withclause w

      | Rename pr -> let+ r = o#pack_rename pr acu in Rename r
    
      | Packnew (id, li, ltypes) ->
        let+ id2 = o#pnew_id id acu
        and+ li2 = o#long_id li
        and+ ltypes2 = list o#ltype ltypes in
        Packnew (id2, li2, ltypes2)

      | Package pc ->

        let+ n = o#packname pc.package_name acu

        (* Package body is a block. *)
        and+ (d,c,io) = mkblock (fun acu ->
            let+ d = list o#pv_declaration pc.package_declarations acu
            and+ c = o#comments pc.package_comments 
            and+ io = option (o#expr S) pc.package_init in
            (d,c,io))
        in
        
        Package { package_name = n ;
                  package_sig = pc.package_sig ;
                  package_declarations = d ;
                  package_comments = c ;
                  package_init = io }
    
      | Typedef (id, args, typeexp, subco) ->
        let+ i = o#type_id id acu
        and+ a = list o#arg args
        and+ t = o#type_expr typeexp
        and+ s = option o#subconstraint subco in
        
        Typedef (i,a,t,s)
        
      | Subtype (id, li, subco) ->
        let+ i = o#type_id id acu
        and+ l = o#subtype li
        and+ s = option o#subconstraint subco in
        
        Subtype (i,l,s)

      | Procdef def -> let+ d = o#procdef def acu in Procdef d
      | Procdecl decl -> let+ d = o#procdecl decl acu in Procdecl d
        
      | Funrename funr ->
        let+ a = o#procdecl funr.fun_alias acu 
        and+ o = o#fun_id funr.fun_orig in
        
        Funrename { fun_alias = a ;
                    fun_orig = o }

      | Vardef vardef ->
        let+ v = o#var_id vardef.varname acu
        and+ t = o#type_expr vardef.vartype 
        and+ c = option o#subconstraint vardef.constrain
        and+ i = option (o#expr S) vardef.vinit in
        
        Vardef { varname = v ;
                 const = vardef.const ;
                 vartype = t ;
                 constrain = c ;
                 vinit = i }

    method type_expr te acu = return te acu

    method subconstraint sub acu = match sub with
      | Index_constraint el -> let+ eel = list (o#expr S) el acu in Index_constraint eel
      | Range_constraint e -> let+ ee = o#expr S e acu in Range_constraint ee

    method procdecl decl acu =
      let+ p = o#procname decl.procname acu
      and+ al = list o#arg decl.args 
      and+ r = option o#rettype decl.rettype in
      
      { procname = p ;
        args = al ;
        rettype = r }

    method arg a acu =
      let+ n = o#argname a.argname acu
      and+ t = o#argtype a.argtype
      and+ eo = option (o#expr S) a.argdefault in
      
      { argname = n ;
        argtype = t ;
        mode = a.mode ;
        argdefault = eo }

    method pack_rename pr acu =
      let+ id = o#pack_id pr.pack_alias acu
      and+ lid = o#long_id pr.pack_orig in
      
      { pack_alias = id ;
        pack_orig = lid }
      
    method procdef def acu =
      
      let+ d = o#procdecl def.decl acu

      (* A function body is a block. *)
      and+ (dl,b,c) = mkblock (fun acu ->
          let+ dl = list o#pv_declaration def.declarations acu
          and+ b = o#expr S def.body
          and+ c = o#comments def.proc_comments
          in
          (dl,b,c))
      in
      
      { decl = d ;
        declarations = dl ;
        body = b ;
        proc_comments = c }

    method ltype (lblo, lid) acu =
      let+ lbl2 = option o#lbl_id lblo acu
      and+ lid2 = o#long_id lid in

      (lbl2, lid2)

    method withclause w acu = match w with
      | With li -> let+ lid = o#with_id li acu in With lid
      | Use li -> let+ lid = o#use_id li acu in With lid
      | Usetype li -> let+ lid = o#usetype_id li acu in With lid

    method content dlpv acu = list o#pv_declaration dlpv acu

    method file fpv acu =

      let+ content = o#content fpv.pv.content acu
      and+ file_comments = o#comments fpv.pv.file_comments in

      let file =
        { path = fpv.pv.path ;
          content ;
          file_comments ;
          fpos = fpv.pv.fpos }
      in
    
      { pv = file ; errors = fpv.errors }
          
  end
