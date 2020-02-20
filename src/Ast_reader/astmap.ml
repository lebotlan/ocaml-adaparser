(*** Generic MAP & FOLD on the AST ***)

(*
 * Accumulators may combine two kinds of behaviors:
 *    - fold-like acus, e.g. counting the number of occurrences of a given variable in the whole program.
 *            such acu are never reset, always propagated
 *
 *    - strictly hierarchival acu, e.g. an acu with the variables currently in scope
 *            such acu are never propagated upwards.
 *
 * An acu may contain two parts having these behaviors. 
 *
 *)
open Astlib
open Parse_errors
open Ast
    
(* Type returned by all mapping methods: a value and a new acu. *)
type ('v,'a) ret =
  { rval : 'v ;
    racu : 'a }

type ('v, 'a) mapper = 'v -> 'a -> ('v, 'a) ret

(* Monadic helper functions *)
let return v a = { rval = v ; racu = a }

(* f returns a plain value, not a ret. *)
let (let+) a f = { rval = f a.rval ; racu = a.racu }
let (and+) ret g = let+ x = g ret.racu in (ret.rval,x)
let (>>=) = (let+)

(* These let= and= get_acu  are used in some ad-hoc cases to store a transitory acu and restore it later with o#up *)
(* f returns a ret. The initial acu is lost (unless saved with get_acu). *)
let (let=) a f = f a.rval
let get_acu acu = return acu acu
let (and=) x y = y { rval = (x.rval, ()) ; racu = x.racu }


let (>>++) ret g = let+ x = g ret.racu in x :: ret.rval

let list f l acu =
  let init = return [] acu in
  (List.fold_left (fun r x -> r >>++ f x) init l) >>= List.rev

let option f x acu =
  match x with
  | None -> return None acu
  | Some y -> let+ k = f y acu in Some k

(*** Mapper classes ***)

(* Note:
 *   nexpr are labeled expressions, like this: (label => expr, ... )
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
  | [] -> S

  | [Value _] -> S    
  | [Id _] -> A (* Probably an argument name. Can still be a constant, though. *)
  | [_] -> S

  (* Multiple labels: it is not an argument *)
  | _ -> S

class ['a] tree_mapper =
  object(o)

    (* upacu : method to be overriden, depending on your acu.
     * Merges input acu and subterm acu 
     * It returns the acu to be used "for the next guy in the list" (e.g. in a declaration list) 
     *
     * fold-like acu    : outacu
     * hierarchical acu : inacu
     *
     * Invariant: o#upacu should be idempotent (no effect when applied twice).
     *            o#upacu acu acu = acu
     *
     * Beware: some methods return a o#up value, some other methods do not (we call them noup methods).
     * when calling a up/noup method, one checks that it is the expected behaviour with respect to hierarchical acu (e.g. namespaces).
    *)
    method up : 'v . 'a -> ('v,'a) ret -> ('v,'a) ret = fun inacu ret -> return ret.rval (o#upacu inacu ret.racu)

    method upacu inacu _outacu = inacu
    
    (* ln : label namespace *)

    (* up method *)
    method expr ln e (acu:'a) = match e with
      | Value v -> o#up acu (let+ v = o#adavalue v acu in Value v)

      | Id li -> o#up acu (let+ li = o#expr_id ln li acu in Id li)
                                 
      | Select (e, id) ->
        o#up acu ( let+ ee = o#expr S e acu
                   and+ iid = o#select_id id in
                   Select (ee, iid))
                            
      | For (oi,rev,id,e1,e2) ->
        o#up acu (let+ ee1 = o#expr S e1 acu                      
                  (* The for identifier scope is e2 only. *)
                  and+ iid = o#for_id id 
                  and+ ee2 = o#expr S e2 in
                  
                  For (oi,rev,iid,ee1,ee2))
        
                                   
      | New (li, el) ->
        o#up acu (let+ lid = o#new_id li acu 
                  and+ l = list (o#expr S) el in
                  New (lid, l))
                          
      | If (e1, e2, e3) ->
        o#up acu (let+ ee1 = o#expr S e1 acu
                  and+ ee2 = o#expr S e2 
                  and+ ee3 = o#expr S e3 in
                  If (ee1, ee2, ee3))
        
      | While (e1, e2) ->
        o#up acu (let+ ee1 = o#expr S e1 acu
                  and+ ee2 = o#expr S e2 in
                  While (ee1, ee2))
          
      | Assign (e1, e2) ->
        o#up acu (let+ ee1 = o#expr S e1 acu 
                  and+ ee2 = o#expr S e2 in
                  Assign (ee1, ee2))

      | Tick (e1, e2) ->
        o#up acu (let+ ee1 = o#expr S e1 acu
                  and+ ee2 = o#expr S e2 in
                  Tick (ee1, ee2))

      | Is_in (e1, e2) ->
        o#up acu (let+ ee1 = o#expr S e1 acu
                  and+ ee2 = o#expr S e2 in
                  Is_in (ee1, ee2))

      | Seq el -> o#up acu (let+ l = list (o#expr S) el acu in Seq l)
                    
      | Tuple nel -> o#up acu (let+ l = list o#nexpr nel acu in Tuple l)

      | App (e, nel) ->
        o#up acu (let+ ee = o#expr S e acu 
                  and+ l = list o#nexpr nel in
                  App (ee, l))

      | Exitwhen e -> o#up acu (let+ ee = o#expr S e acu in Exitwhen ee)
                        
      | Return e -> o#up acu (let+ ee = o#expr S e acu in Return ee)

      | Declare (dl, e) ->
        o#up acu (let+ ll = list o#declaration dl acu
                  and+ ee = o#expr S e in
                  Declare (ll, ee))

      | Case (e, ws) ->
        o#up acu (let+ ee = o#expr S e acu 
                  and+ ww = list o#whenc ws in
                  Case (ee, ww))

      | Try (e, ws) -> 
        o#up acu (let+ ee = o#expr S e acu 
                  and+ ww = list o#whenc ws in
                  Try (ee, ww))
                         
      | Unconstrained -> return Unconstrained acu

      | Interval (e1, e2) ->
        o#up acu (let+ ee1 = o#expr S e1 acu 
                  and+ ee2 = o#expr S e2 in
                  Interval (ee1, ee2))
          
      | Range (e1, e2) ->
        o#up acu (let+ ee1 = o#expr S e1 acu 
                  and+ ee2 = o#expr S e2 in
                  Range (ee1, ee2))
                               
      | TickRange (e, avo) -> o#up acu (let+ ee = o#expr S e acu 
                                        and+ vvo = option o#adavalue avo in
                                        TickRange (ee, vvo))

    (* noup *)
    method adavalue v acu = return v acu

    (* noup *)
    method whenc (Match (ido, el, e)) acu =
      let+ ido2 = option o#when_id ido acu
      and+ l = list (o#expr S) el 
      and+ ee = o#expr S e in
      Match (ido2, l, ee)

    (* noup but the acu is reset anyway since there are only calls to o#expr, which is up *)
    method nexpr (lbl, e) acu =
      let k = find_nexpr_kind lbl in      
      let+ a = list (o#expr k) lbl acu
      and+ b = o#expr S e in
      (a,b)
    
    (* noup *)

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

    (* pv_declaration = declaration list pv 
     * noup *)
    method pv_declaration dlpv acu =
      let+ dl = list o#declaration dlpv.pv acu in
      { pv = dl ; errors = dlpv.errors }
    
    (* noup (!) A declaration must be available by the subsequent elements. *)
    method declaration dl acu = match dl with      
      | Withclause wc -> let+ w = o#withclause wc acu in Withclause w

      | Rename pr -> let+ r = o#pack_rename pr acu in Rename r
    
      | Packnew (id, li, ltypes) ->
        let+ id2 = o#pnew_id id acu
        and+ li2 = o#long_id li
        and+ ltypes2 = list o#ltype ltypes in
        Packnew (id2, li2, ltypes2)

      | Package pc ->

        let= n = o#packname pc.package_name acu
        and+ acu1 = get_acu in
        
        (* #up here, to prevent embedded declarations from going upwards. *)
        let+ d = list o#pv_declaration pc.package_declarations acu
        and+ c = o#comments pc.package_comments 
        and+ io = option (o#expr S) pc.package_init
        and= () = o#up acu1 in
          
        Package { package_name = n ;
                  package_sig = pc.package_sig ;
                  package_declarations = d ;
                  package_comments = c ;
                  package_init = io }
    
      | Typedef (id, args, typeexp, subco) ->
        let+ i = o#type_id id acu
        and+ a = list o#arg args
        and+ t = o#type_expr typeexp
        and+ s = option o#subconstraint subco
        in
        Typedef (i,a,t,s)
        
      | Subtype (id, li, subco) ->
        let+ i = o#type_id id acu
        and+ l = o#subtype li
        and+ s = option o#subconstraint subco
        in
        Subtype (i,l,s)

      | Procdef def -> let+ d = o#procdef def acu in Procdef d
      | Procdecl decl -> let+ d = o#procdecl decl acu in Procdecl d
        
      | Funrename funr ->
        let+ a = o#procdecl funr.fun_alias acu 
        and+ o = o#fun_id funr.fun_orig
        in
        Funrename { fun_alias = a ;
                    fun_orig = o }

      | Vardef vardef ->
        let+ v = o#var_id vardef.varname acu
        and+ t = o#type_expr vardef.vartype 
        and+ c = option o#subconstraint vardef.constrain
        and+ i = option (o#expr S) vardef.vinit
        in
        Vardef { varname = v ;
                 const = vardef.const ;
                 vartype = t ;
                 constrain = c ;
                 vinit = i }

    method type_expr te acu = return te acu

    method subconstraint sub acu = match sub with
      | Index_constraint el -> let+ eel = list (o#expr S) el acu in Index_constraint eel
      | Range_constraint e -> let+ ee = o#expr S e acu in Range_constraint ee

    (* noup *)
    method procdecl decl acu =
      let+ p = o#procname decl.procname acu
      and+ al = list o#arg decl.args 
      and+ r = option o#rettype decl.rettype
      in
      { procname = p ;
        args = al ;
        rettype = r }

    method arg a acu =
      let+ n = o#argname a.argname acu
      and+ t = o#argtype a.argtype
      and+ eo = option (o#expr S) a.argdefault
      in
      { argname = n ;
        argtype = t ;
        mode = a.mode ;
        argdefault = eo }

    method pack_rename pr acu =
      let+ id = o#pack_id pr.pack_alias acu
      and+ lid = o#long_id pr.pack_orig
      in
      { pack_alias = id ;
        pack_orig = lid }
      

    (* noup? *)
    method procdef def acu =
      
      let= d = o#procdecl def.decl acu
      and+ acu1 = get_acu in

      let+ dl = list o#pv_declaration def.declarations acu1
      and+ b = o#expr S def.body
      and+ c = o#comments def.proc_comments

      (* Apply o#up on the return value. *)
      and= () = o#up acu1 in
      
      { decl = d ;
        declarations = dl ;
        body = b ;
        proc_comments = c }

    (* noup *)
    method ltype (lblo, lid) acu =
      let+ lbl2 = option o#lbl_id lblo acu
      and+ lid2 = o#long_id lid
      in
      (lbl2, lid2)

    (* noup *)
    method withclause w acu = match w with
      | With li -> let+ lid = o#with_id li acu in With lid
      | Use li -> let+ lid = o#use_id li acu in With lid
      | Usetype li -> let+ lid = o#usetype_id li acu in With lid

    method content dlpv acu = list o#pv_declaration dlpv acu

  end
