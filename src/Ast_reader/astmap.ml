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
 * The user is expected to provide several functions in order to define how acu data is propagated:
 *      block_exit  : what happens when exiting the current block (a block is e.g. a then part or a else part in C or Java: one may define local variables there. In Ada blocks are more explicit, though.)
 *      merge       : what happens when two control flows merge.
 *                    the function merge actually consists in three parts: merge_pre, merge_mid, merge_end (see below)
 *
 *)
open Astlib
open Parse_errors
open Ast
open Loc
    
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
 *   - merge_pre takes acu0 and returns acu1
 *   - merge_mid takes acu0, acu1' and returns acu2
 *   - merge_end takes acu0, acu1', acu2', and returns the final acu.
 *
 *  (Note that both left & right functions called by merge actually build a new acu _and_ a new value.
 *   The new value is used internally to built the subtree, but is eventually hidden from the merge functions.
 *   Hence, the real control flow is a bit intricate, but hopefully nicely hidden from the user).
 *
 * *)
type 'a user_fun =
  { (* It receives the input acu (when entering the block) and output acu (once the block is finished). 
     * It must return the final acu. *)
    block_exit: 'a -> 'a -> 'a ;

    (*
     * acu0 is the initial acu (before branches)
     * acu1 is the acu result of the left branch.
     * acu2 is the acu result of the right branch. 
     *)
    merge_pre: acu0:'a -> 'a ;
    merge_mid: acu0:'a -> acu1:'a -> 'a ;
    merge_end: acu0:'a -> acu1:'a -> acu2:'a -> 'a ;

    (* Note : merge_mid is highly expected to behave like block_exit *)
    
  }

let accumulates =
  let merge_pre ~acu0 = acu0
  and merge_mid ~acu0:_ ~acu1 = acu1
  and merge_end ~acu0:_ ~acu1:_ ~acu2 = acu2
  and block_exit _inacu outacu = outacu in

  { block_exit ; merge_pre ; merge_mid ; merge_end }

let scoped =
  let merge_pre ~acu0 = acu0
  and merge_mid ~acu0 ~acu1:_ = acu0
  and merge_end ~acu0 ~acu1:_ ~acu2:_ = acu0
  and block_exit inacu _outacu = inacu in

  { block_exit ; merge_pre ; merge_mid ; merge_end }

(* Monadic helper functions *)
let return v acu = { rval = v ; acu }

let mapacu rv f = { rval = rv.rval ; acu = f rv.acu }

(* k builds an unboxed value. *)
let (let+) r k = { r with rval = k r.rval }
let (and+) r g = let+ x = g r.acu in (r.rval,x)

(* k builds a boxed value & expects an acu. *)
let (let=) r k = k r.rval r.acu

(* k returns only a new acu. We keep the value returned in the let. *)
let (let-) r k =  { r with acu = k (r.rval, r.acu) }

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

(* Kind of arguments *)
type arg_kind =
  (* Argument of a procedure / function. 
   * The boolean indicates if this is a binder.
   *    A procdecl does not bind its arguments.
   *    A procdef binds the arguments in its body.
   *   
   *    A procdef first invokes #arg with Proc_arg false (for the declaration),    (A)
   *    then it invokes #arg with Proc_arg true for the body <- with the mapped arguments returned by (A)
   *         Proc_arg true => it is the second time these arguments are seen, and these may have already been mapped.
   *                          they are not supposed to be mapped again
   *  
   *)
  | Proc_arg of bool

  (* Argument in a typedef (not a binder) *)
  | Typedef_arg

(* #procdecl may be invoked in three different contexts *)
type procdecl_kind =
  (* Only a declaration, in a package spec. *)
  | Pdk_only_decl

  (* A function renaming (procdecl is invoked) *)
  | Pdk_funrename

  (* In a procdef *)
  | Pdk_procdef

(* In which context declarations are found. *)
type dkind =
  | Toplevel
  | In_package
  | In_declare
  | In_proc

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

  | [ { v = Value _ ; _ } ] -> S    
  | [ { v = Id _ ; _ } ] -> A (* Probably an argument name. Can still be a constant, though. *)
  | [ _ ] -> S

  (* Multiple labels: it is not an argument *)
  | _ -> S


(* block_exit receives the acu of the currently finished block and the initial value of the acu when entering this block. 
   merge receives two acus corresponding to two merging control-flows.
*)
class ['a] tree_mapper user_fun =

  (* Merges two branches. *)

  (* Strange bug: if let++ is defined here, compilation fails with some error. 
   * Instead, we define a nice-looking letpp function, and we have to define let++ each time we need it. *)
  let letpp (f,g) k acu0 =

    let rx = f (user_fun.merge_pre ~acu0) in
    let ry = g (user_fun.merge_mid ~acu0 ~acu1:rx.acu) in
    let final_acu = user_fun.merge_end ~acu0 ~acu1:rx.acu ~acu2:ry.acu in
       
    return (k (rx.rval, ry.rval)) final_acu
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

  let block inacu ret = return ret.rval (user_fun.block_exit inacu ret.acu) in

  let mkblock f = fun acu -> block acu (f acu) in

  object(o)

    (* Possible optimization: the tree is duplicated here, even if the mapping is the identity.
     *    => one should check, at each node, if new childs == old childs, then return old subtree instead of a copy. *)
    
    (* ln  means label namespace *)

    method block: 'v . 'a -> ('v,'a) ret -> ('v,'a) ret = fun acu ret -> block acu ret

    method declare_expr e acu = o#expr S e acu
    method proc_body e acu = o#expr S e acu

    method expr ln e acu =
      let+ v = o#core_expr e.pos ln e.v acu in
      { v ; pos = e.pos }
    
    method core_expr _pos ln e acu =
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

      | Tick (e, l) ->
        let+ ee = o#expr S e acu
        and+ ll = o#attribute l in
        Tick (ee, ll)

      | Is_in (e1, e2) ->
        let+ ee1 = o#expr S e1 acu
        and+ ee2 = o#expr S e2 in
        Is_in (ee1, ee2)

      | Seq (ord,el) -> let+ l = list (o#expr S) el acu in Seq (ord,l)

      | Tuple nel -> let+ l = list o#nexpr nel acu in Tuple l

      | App (e, nel) -> let+ (e2, nel2) = o#app (e, nel) acu in App (e2, nel2)

      | Exitwhen e -> let+ ee = o#expr S e acu in Exitwhen ee

      | Return e -> let+ ee = o#expr S e acu in Return ee

      | Declare (dl,e) -> let+ (dl2, e2) = o#declare (dl, e) acu in Declare (dl2, e2)

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

    method declare (dl, e) acu =
      block acu
        (let+ ll = o#pl_declarations In_declare dl acu
         and+ ee = o#declare_expr e in
         (ll, ee))
    
    (* At parse-time, only ground values can appear: numbers, arrays, tuples (records),
     * but no builtin or function. *)
    method adavalue v acu = return v acu

    method app (e, nel) acu =
      let+ ee = o#expr S e acu 
      and+ l = list o#nexpr nel in
      (ee, l)
    
    method whenc (Match (ido, el, e)) acu =      
      let+ l = list (o#expr S) el acu

      (* ido binds a new name. There is a block here. *)
      and+ (ido2, ee) = mkblock (fun acu ->
          let+ ido2 = option o#when_id ido acu
          and+ ee = o#expr S e in
          (ido2, ee))
      in
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
    
    (* pack rename, pack new, ltype, ... *)
    method long_id li acu = return li acu

    method typename = o#long_id

    (* Subtype t is xxx (this is xxx): *)
    method subtype = o#long_id
    method fun_id = o#long_id    
    method new_id = o#long_id
    method use_id = o#long_id
    method usetype_id = o#long_id

    (* with is not supposed to be influenced by the context. *)
    method with_id li acu = return li acu
    
    method select_id id acu = return id acu

    method attribute id acu = return id acu

    (* Binders *)
    method argname _kind id acu = return id acu
    method var_id id acu = return id acu
    method for_id id acu = return id acu
    method when_id id acu = return id acu
    method enumerate_value id acu = return id acu
    
    (* Package rename *)
    method pack_id id acu = return id acu
        
    method pnew_id id acu = return id acu
    method packname id acu = return id acu
    method procname _kind id acu = return id acu
    method type_id id acu = return id acu    
       
    (*** Declarations ***)

    (* pl_declaration = declaration list pv *)
    method pl_declarations kind dlp acu =
      let+ dl = list (o#declaration kind) dlp.pv acu in
      { pv = dl ; errors = dlp.errors }

    method packnew (id, li, ltypes) acu =
      let+ id2 = o#pnew_id id acu
      and+ li2 = o#long_id li
      and+ ltypes2 = list o#ltype ltypes in
      (id2, li2, ltypes2)
    
    method declaration kind dl acu = match dl with      
      | Withclause wc -> let+ w = o#withclause wc acu in Withclause w

      | Rename pr -> let+ r = o#pack_rename pr acu in Rename r
      | Funrename fr -> let+ r = o#fun_rename fr acu in Funrename r
    
      | Packnew pnew -> let+ p = o#packnew pnew acu in Packnew p

      | Package pc ->

        let+ n = o#packname pc.package_name acu

        (* Package body is a block. *)
        and+ (d,c,io) = mkblock (fun acu ->
            let+ d = o#pl_declarations In_package pc.package_declarations acu
            and+ c = o#comments pc.package_comments 
            and+ io = option (o#expr S) pc.package_init in
            (d,c,io))
        in
        
        Package { package_name = n ;
                  package_sig = pc.package_sig ;
                  package_declarations = d ;
                  package_comments = c ;
                  package_init = io }
    
      | Typedef typedef -> let+ td = o#typedef typedef acu in Typedef td        
      | Subtype subtypedef -> let+ std = o#subtypedef subtypedef acu in Subtype std
      | Procdef def -> let+ d = o#procdef def acu in Procdef d
      | Procdecl decl -> let+ d = o#procdecl Pdk_only_decl decl acu in Procdecl d
        
      | Vardef vardef -> let+ vd = o#vardef kind vardef acu in Vardef vd

    method typedef td acu =
      let+ i = o#type_id td.t_name acu
      and+ a = list (o#arg Typedef_arg) td.t_args
      and+ t = o#type_expr td.t_body
      and+ s = option o#subconstraint td.t_constrain in
      
      { t_name = i ;
        t_args = a ;
        t_body = t ;
        t_constrain = s }

    method subtypedef std acu =
      let+ i = o#type_id std.st_name acu
      and+ l = o#subtype std.st_typ
      and+ s = option o#subconstraint std.st_constrain in
        
      { st_name = i ;
        st_typ = l ;
        st_constrain = s }
      
    method vardef _kind v a = o#core_vardef v a
    method record_field f a = o#core_vardef f a

    (* Used by vardef and record_field *)
    method core_vardef vardef acu =
      let+ v = o#var_id vardef.varname acu
      and+ t = o#type_expr vardef.vartype 
      and+ c = option o#subconstraint vardef.constrain
      and+ i = option (o#expr S) vardef.vinit in
      
      { varname = v ;
        const = vardef.const ;
        vartype = t ;
        constrain = c ;
        vinit = i }
    
    (* Invoked for variable definition, type definitions, ... *)
    method type_expr te acu = match te with
      | Abstract -> return te acu
      | Typename li -> let+ n = o#typename li acu in Typename n
      | Enumerate en -> let+ en = list o#enumerate_value en acu in Enumerate en
      | Record vdl -> let+ vdl = list o#record_field vdl acu in Record vdl
      | Array (rgs, cell) -> let+ rgs = list (o#expr S) rgs acu and+ n = o#typename cell in Array (rgs, n)
      | Delta (v1, v2) -> let+ v1 = o#adavalue v1 acu and+ v2 = o#adavalue v2 in Delta (v1, v2)

    method subconstraint sub acu = match sub with
      | Index_constraint el -> let+ eel = list (o#expr S) el acu in Index_constraint eel
      | Range_constraint e -> let+ ee = o#expr S e acu in Range_constraint ee    

    method procdecl kind decl acu =
      let+ p = o#procname kind decl.procname acu
      and+ al = list (o#arg (Proc_arg false)) decl.args 
      and+ r = option o#typename decl.rettype in
      
      { procname = p ;
        args = al ;
        rettype = r }

    method arg kind a acu =
      let+ n = o#argname kind a.argname acu
      and+ t = o#typename a.argtype
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

    method fun_rename fr acu =
      let+ a = o#procdecl Pdk_funrename fr.fun_alias acu 
      and+ o = o#fun_id fr.fun_orig in
      
      { fun_alias = a ;
        fun_orig = o }

    
    method procdef def acu =
      
      let= d = o#procdecl Pdk_procdef def.decl acu in

      begin fun acu ->
        (* A function body is a block. *)
        let+ (dl,b,c) = mkblock (fun acu ->
            
            (* Consider now the arguments as binders.
             * They should not be mapped again by #arg. *)
            let+ al = list (o#arg (Proc_arg true)) d.args acu
                
            and+ dl = o#pl_declarations In_proc def.declarations
            and+ b = o#proc_body def.body
            and+ c = o#comments def.proc_comments
            in
            
            assert (al = d.args) ; (* Arguments must not be mapped again. They should have been mapped in procdecl *)
            
            (dl,b,c)) acu
        in
      
        { decl = d ;
          declarations = dl ;
          body = b ;
          proc_comments = c }
      end
        
    method ltype (lblo, lid) acu =
      let+ lbl2 = option o#lbl_id lblo acu
      and+ lid2 = o#long_id lid in

      (lbl2, lid2)

    method withclause w acu = match w with
      | With li -> let+ lid = o#with_id li acu in With lid
      | Use li -> let+ lid = o#use_id li acu in Use lid
      | Usetype li -> let+ lid = o#usetype_id li acu in Usetype lid

    method content dlpv acu = o#pl_declarations Toplevel dlpv acu

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

class ['b] short_mapper (tmapper: 'a tree_mapper) (f:'a -> 'b) (g:'b -> 'a) =

  let rmap r = { rval = r.rval ; acu = f r.acu } in
  let map h = fun arg1 arg2 -> rmap (h arg1 (g arg2)) in
  
  object
    method file = map tmapper#file
    method procdef = map tmapper#procdef
  end
