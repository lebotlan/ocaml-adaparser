(*** Generic MAP & FOLD on AST ***)

(*
 * Accumulators may combine two kinds of behaviors:
 *    - fold-like acus, e.g. counting the number of occurrences of a given variable in the whole program.
 *            such acu are never reset, always propagated
 *
 *    - strictly hierarchival acu, e.g. an acu with the variables currently in scope
 *            such acu are never propagated upwards.
 *
 *)
open Astlib
open Parse_errors
open Ast
    
(* Type returned by all mapping methods: a value and a new acu. *)
type ('a,'b) ret =
  { rval : 'a ;
    racu : 'b }

type ('b, 'a) mapper = 'b -> 'a -> ('b, 'a) ret

(* Monadic helper functions *)
let return v a = { rval = v ; racu = a }

let rmap a f = { rval = f a.rval ; racu = a.racu }
let (>>=) = rmap

let rjoin ret g = g ret.racu >>= (fun x -> (ret.rval,x))
let (>>+) = rjoin

let (>>++) ret g = g ret.racu >>= (fun x -> x :: ret.rval)

let list f l acu =
  let init = return [] acu in
  (List.fold_left (fun r x -> r >>++ f x) init l) >>= List.rev

let option f x acu =
  match x with
  | None -> return None acu
  | Some y -> f y acu >>= (fun k -> Some k)

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
    method up : 'b . 'a -> ('b,'a) ret -> ('b,'a) ret = fun inacu ret -> return ret.rval (o#upacu inacu ret.racu)

    method upacu inacu _outacu = inacu
    
    (* ln : label namespace *)

    (* up method *)
    method expr ln e (acu:'a) = match e with
      | Value v -> o#up acu (o#adavalue v acu >>= (fun v -> Value v))

      | Id li -> o#up acu (o#expr_id ln li acu >>= (fun li -> Id li))
                                 
      | Select (e, id) ->
        o#up acu (o#expr S e acu >>+
                  o#select_id id >>=
                  (fun (ee, iid) -> Select (ee, iid)))
                            
      | For (oi,rev,id,e1,e2) ->
        o#up acu (o#expr S e1 acu >>+

                  (* The for identifier scope is e2 only. *)
                  o#for_id id >>+ 
                  o#expr S e2 >>=
                  (fun ((ee1, iid), ee2) -> For (oi,rev,iid,ee1,ee2)))
                                   
      | New (li, el) ->
        o#up acu (o#new_id li acu >>+
                  list (o#expr S) el >>=
                  (fun (lid, l) -> New (lid, l)))
                          
      | If (e1, e2, e3) ->
        o#up acu (o#expr S e1 acu >>+
                  o#expr S e2 >>+
                  o#expr S e3 >>=
                  (fun ((ee1, ee2), ee3) -> If (ee1, ee2, ee3)))
        
      | While (e1, e2) ->
        o#up acu (o#expr S e1 acu >>+
                  o#expr S e2 >>=
                  (fun (ee1, ee2) -> While (ee1, ee2)))
          
      | Assign (e1, e2) ->
        o#up acu (o#expr S e1 acu >>+
                  o#expr S e2 >>=
                  (fun (ee1, ee2) -> Assign (ee1, ee2)))

      | Tick (e1, e2) ->
        o#up acu (o#expr S e1 acu >>+
                  o#expr S e2 >>=
                  (fun (ee1, ee2) -> Tick (ee1, ee2)))

      | Is_in (e1, e2) ->
        o#up acu (o#expr S e1 acu >>+
                  o#expr S e2 >>=
                  (fun (ee1, ee2) -> Is_in (ee1, ee2)))

      | Seq el -> o#up acu (list (o#expr S) el acu >>= (fun l -> Seq l))
                    
      | Tuple nel -> o#up acu (list o#nexpr nel acu >>= (fun l -> Tuple l))

      | App (e, nel) ->
        o#up acu (o#expr S e acu >>+
                  list o#nexpr nel >>=
                  (fun (ee, l) -> App (ee, l)))

      | Exitwhen e -> o#up acu (o#expr S e acu >>= (fun ee -> Exitwhen ee))
                        
      | Return e -> o#up acu (o#expr S e acu >>= (fun ee -> Return ee))

      | Declare (dl, e) ->
        o#up acu (list o#declaration dl acu >>+
                  o#expr S e >>=
                  (fun (ll, ee) -> Declare (ll, ee)))

      | Case (e, ws) ->
        o#up acu (o#expr S e acu >>+
                  list o#whenc ws >>=
                  (fun (ee, ww) -> Case (ee, ww)))

      | Try (e, ws) -> 
        o#up acu (o#expr S e acu >>+
                  list o#whenc ws >>=
                  (fun (ee, ww) -> Try (ee, ww)))
                         
      | Unconstrained -> return Unconstrained acu

      | Interval (e1, e2) ->
        o#up acu (o#expr S e1 acu >>+
                  o#expr S e2 >>=
                  (fun (ee1, ee2) -> Interval (ee1, ee2)))
          
      | Range (e1, e2) ->
        o#up acu (o#expr S e1 acu >>+
                  o#expr S e2 >>=
                  (fun (ee1, ee2) -> Range (ee1, ee2)))
                               
      | TickRange (e, avo) -> o#up acu (o#expr S e acu >>+
                                        option o#adavalue avo >>=
                                        (fun (ee, vvo) -> TickRange (ee, vvo)))

    (* noup *)
    method adavalue v acu = return v acu

    (* noup *)
    method whenc (Match (ido, el, e)) acu =
      option o#when_id ido acu >>+
      list (o#expr S) el >>+
      o#expr S e >>=
      (fun ((ido2, l), ee) -> Match (ido2, l, ee))

    (* noup but the acu is reset anyway since there are only calls to o#expr, which is up *)
    method nexpr (lbl, e) acu =
      let k = find_nexpr_kind lbl in      
      list (o#expr k) lbl acu >>+
      o#expr S e >>=
      (fun p -> p)
    
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
      list o#declaration dlpv.pv acu >>=
      (fun dl -> { pv = dl ; errors = dlpv.errors } )
    
    (* noup (!) A declaration must be available by the subsequent elements. *)
    method declaration dl acu = match dl with      
      | Withclause wc -> o#withclause wc acu >>= (fun w -> Withclause w)

      | Rename pr -> o#pack_rename pr acu >>= (fun r -> Rename r)
    
      | Packnew (id, li, ltypes) ->
        o#pnew_id id acu >>+
        o#long_id li >>+
        list o#ltype ltypes >>=
        (fun ((id2, li2), ltypes2) -> Packnew (id2, li2, ltypes2))

      | Package pc ->
        o#packname pc.package_name acu >>+

        (* #up here, to prevent embedded declarations to go upwards. *)
        (fun acu -> o#up acu
            (list o#pv_declaration pc.package_declarations acu >>+
             o#comments pc.package_comments >>+
             option (o#expr S) pc.package_init)) >>=
        
        (fun (n, ((d, c), io)) -> Package { package_name = n ;
                                            package_sig = pc.package_sig ;
                                            package_declarations = d ;
                                            package_comments = c ;
                                            package_init = io })
    
      | Typedef (id, args, typeexp, subco) ->
        o#type_id id acu >>+
        list o#arg args >>+
        o#type_expr typeexp >>+
        option o#subconstraint subco >>=
        (fun (((i,a),t),s) -> Typedef (i,a,t,s))
        
      | Subtype (id, li, subco) ->
        o#type_id id acu >>+
        o#subtype li >>+
        option o#subconstraint subco >>=
        (fun ((i,l),s) -> Subtype (i,l,s))

      | Procdef def -> o#procdef def acu >>= (fun d -> Procdef d)
      | Procdecl decl -> o#procdecl decl acu >>= (fun d -> Procdecl d)
        
      | Funrename funr ->
        o#procdecl funr.fun_alias acu >>+
        o#fun_id funr.fun_orig >>=
        (fun (a,o) -> Funrename { fun_alias = a ;
                                  fun_orig = o })

      | Vardef vardef ->
        o#var_id vardef.varname acu >>+
        o#type_expr vardef.vartype >>+
        option o#subconstraint vardef.constrain >>+
        option (o#expr S) vardef.vinit >>=
        ( fun (((v, t), c), i) -> Vardef { varname = v ;
                                           const = vardef.const ;
                                           vartype = t ;
                                           constrain = c ;
                                           vinit = i })

    method type_expr te acu = return te acu

    method subconstraint sub acu = match sub with
      | Index_constraint el -> list (o#expr S) el acu >>= (fun eel -> Index_constraint eel)
      | Range_constraint e -> o#expr S e acu >>= (fun ee -> Range_constraint ee)

    (* noup *)
    method procdecl decl acu =
      o#procname decl.procname acu >>+
      list o#arg decl.args >>+
      option o#rettype decl.rettype >>=
      (fun ((p, al), r) ->  { procname = p ;
                              args = al ;
                              rettype = r })

    method arg a acu =
      o#argname a.argname acu >>+
      o#argtype a.argtype >>+
      option (o#expr S) a.argdefault >>=
      (fun ((n,t),eo) -> { argname = n ;
                           argtype = t ;
                           mode = a.mode ;
                           argdefault = eo })

    method pack_rename pr acu =
      o#pack_id pr.pack_alias acu >>+
      o#long_id pr.pack_orig >>=
      (fun (id, lid) -> { pack_alias = id ;
                          pack_orig = lid })
      

    (* noup *)
    method procdef def acu =
      o#procdecl def.decl acu >>+

      (fun acu -> o#up acu
          (list o#pv_declaration def.declarations acu >>+
           o#expr S def.body >>+
           o#comments def.proc_comments)) >>=
      
      (fun (d, ((dl,b),c)) -> { decl = d ;
                                declarations = dl ;
                                body = b ;
                                proc_comments = c })

    (* noup *)
    method ltype (lblo, lid) acu =
      option o#lbl_id lblo acu >>+
      o#long_id lid >>=
      (fun (lbl2, lid2) -> (lbl2, lid2))

    (* noup *)
    method withclause w acu = match w with
      | With li -> o#with_id li acu >>= (fun lid -> With lid)
      | Use li -> o#use_id li acu >>= (fun lid -> With lid)
      | Usetype li -> o#usetype_id li acu >>= (fun lid -> With lid)

    method content dlpv acu = list o#pv_declaration dlpv acu

  end

