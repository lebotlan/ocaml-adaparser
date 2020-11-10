open Ast
open Adavalues
open Idents

let cmp = Stdlib.compare

let cmp_pair x y = if x = 0 then y else x

let icmp i1 i2 = Loc.(cmp i1.v i2.v)

let rec cmp_list f l1 l2 = match (l1, l2) with
  | [], [] -> 0
  | _ :: _, [] -> 1
  | [], _ :: _ -> -1
  | x :: xs, y :: ys ->
    let r = f x y in
    if r = 0 then cmp_list f xs ys else r

let rec cmp_all = function
  | [] -> 0
  | 0 :: xs -> cmp_all xs
  | x :: _ -> x

let cmp_opt f o1 o2 = match (o1, o2) with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some x, Some y -> f x y

let rec cmp_core_expr e1 e2 = match (e1, e2) with
  | Value v1, Value v2 -> Adavalue.cmp v1 v2
  | Id i1, Id i2 -> icmp i1 i2
  | Qualified (li1,i1), Qualified (li2,i2) -> cmp_pair (li_cmp li1 li2) (icmp i1 i2)
  | Tuple nel1, Tuple nel2 -> cmp_list cmp_nexpr nel1 nel2
                                
  | Assign (e1a, e1b), Assign (e2a, e2b)
  | While (e1a, e1b), While (e2a, e2b)
  | Interval (e1a, e1b), Interval (e2a, e2b)
  | Range (e1a, e1b), Range (e2a, e2b)
  | Is_in (e1a, e1b), Is_in (e2a, e2b)
  | Typetick (e1a, e1b), Typetick (e2a, e2b) -> cmp_pair (cmp_expr e1a e2a) (cmp_expr e1b e2b)
                                 
  | App (e1, l1), App (e2, l2) -> cmp_pair (cmp_expr e1 e2) (cmp_list cmp_nexpr l1 l2)

  | Select (e1, i1), Select (e2, i2)
  | Tick (e1, i1), Tick (e2, i2)-> cmp_pair (cmp_expr e1 e2) (icmp i1 i2)

  | Return  e1, Return e2
  | Exitwhen e1, Exitwhen e2 -> cmp_expr e1 e2
                                  
  | If (e1a, e1b, e1c), If (e2a, e2b, e2c) -> cmp_lexpr [ e1a ; e1b ; e1c ] [ e2a ; e2b ; e2c ]

  | For (w1, b1, i1, e1a, e1b), For (w2, b2, i2, e2a, e2b) ->
    cmp_all [ cmp w1 w2 ; cmp b1 b2 ; icmp i1 i2 ; cmp_expr e1a e2a ; cmp_expr e1b e2b ]

  | Unconstrained, Unconstrained -> 0

  | New (li1, el1), New (li2, el2) -> cmp_pair (li_cmp li1 li2) (cmp_lexpr el1 el2)

  | Seq (true, el1), Seq (true, el2) -> cmp_lexpr el1 el2
                                          
  | Seq (false, el1), Seq (false, el2) ->
    let el1 = List.sort cmp_expr el1
    and el2 = List.sort cmp_expr el2 in
    cmp_lexpr el1 el2

  | TickRange (e1, ov1), TickRange (e2, ov2) -> cmp_pair (cmp_expr e1 e2) (cmp_opt Adavalue.cmp ov1 ov2)
                        
  | Declare (pld1, e1), Declare (pld2, e2) -> cmp_pair (cmp_pl_decl pld1 pld2) (cmp_expr e1 e2)

  | Case (e1, wcl1), Case (e2, wcl2)
  | Try (e1, wcl1), Try (e2, wcl2) -> cmp_pair (cmp_expr e1 e2) (cmp_list cmp_when_clause wcl1 wcl2)


  | _, _ -> cmp e1 e2


and cmp_expr e1 e2 = cmp_core_expr e1.v e2.v

and cmp_lexpr l1 l2 = cmp_list cmp_expr l1 l2
    
and cmp_nexpr (nel1, e1) (nel2, e2) = cmp_pair (cmp_lexpr nel1 nel2) (cmp_expr e1 e2)

and cmp_when_clause wc1 wc2 = match (wc1, wc2) with
  | Match (io1, el1, e1), Match (io2, el2, e2) -> cmp_all [ (cmp_opt icmp io1 io2) ; (cmp_lexpr el1 el2) ; (cmp_expr e1 e2) ]

                                    
and cmp_pl_decl pld1 pld2 = cmp_list cmp_decl pld1.pv pld2.pv

and cmp_decl d1 d2 = match (d1,d2) with
  | Withclause w1, Withclause w2 -> cmp_with w1 w2
  | Rename { pack_alias = pa1 ; pack_orig = po1 }, Rename { pack_alias = pa2 ; pack_orig = po2 } -> cmp_pair (icmp pa1 pa2) (li_cmp po1 po2)

  | Packnew (i1, li1, ltl1), Packnew (i2, li2, ltl2) -> cmp_all [ (icmp i1 i2) ; (li_cmp li1 li2) ; (cmp_list cmp_ltype ltl1 ltl2) ]

  | Package pc1, Package pc2 -> cmp_pack pc1 pc2
                                  
  | Typedef td1, Typedef td2 -> cmp_typedef td1 td2      
  | Subtype std1, Subtype std2 -> cmp_subtypedef std1 std2
  | Vardef vd1, Vardef vd2 -> cmp_vardef vd1 vd2

  | Funrename { fun_alias = pd1 ; fun_orig = li1 }, Funrename { fun_alias = pd2 ; fun_orig = li2 } -> cmp_pair (cmp_procdecl pd1 pd2) (li_cmp li1 li2)
  | Procdecl pd1, Procdecl pd2 -> cmp_procdecl pd1 pd2
  | Procdef pd1, Procdef pd2 -> cmp_procdef pd1 pd2
  | _ -> cmp d1 d2


and cmp_with w1 w2 = match (w1, w2) with
  | With li1, With li2
  | Use li1, Use li2
  | Usetype li1, Usetype li2 -> li_cmp li1 li2
  | _ -> cmp w1 w2

and cmp_ltype (io1, li1) (io2, li2) = cmp_pair (cmp_opt icmp io1 io2) (li_cmp li1 li2)

and cmp_pack pc1 pc2 =
  cmp_all [ (li_cmp pc1.package_name pc2.package_name) ;
            (cmp pc1.package_sig pc2.package_sig) ;
            (cmp_pl_decl pc1.package_declarations pc2.package_declarations) ;
            (cmp pc1.package_comments pc2.package_comments) ;
            (cmp_opt cmp_expr pc1.package_init pc2.package_init) ]
  
and cmp_typedef t1 t2 =
  cmp_all [ (icmp t1.t_name t2.t_name) ;
            (cmp_list cmp_arg t1.t_args t2.t_args) ;
            (cmp_type_expr t1.t_body t2.t_body) ;
            (cmp_opt cmp_subt_constraint t1.t_constrain t2.t_constrain) ]

and cmp_subtypedef st1 st2 =
  cmp_all [ (icmp st1.st_name st2.st_name) ;
            (li_cmp st1.st_typ st2.st_typ) ;
            (cmp_opt cmp_subt_constraint st1.st_constrain st2.st_constrain) ]

and cmp_arg a1 a2 =
  cmp_all [ (icmp a1.argname a2.argname) ;
            (li_cmp a1.argtype a2.argtype) ;
            (cmp a1.mode a2.mode) ;
            (cmp_opt cmp_expr a1.argdefault a2.argdefault) ]

and cmp_subt_constraint s1 s2 = match (s1, s2) with
  | Index_constraint el1, Index_constraint el2 -> cmp_lexpr el1 el2
  | Range_constraint e1, Range_constraint e2 -> cmp_expr e1 e2
  | _ -> cmp s1 s2

and cmp_type_expr t1 t2 = match (t1, t2) with
  | Abstract, Abstract -> 0
  | Typename li1, Typename li2 -> li_cmp li1 li2
  | Enumerate il1, Enumerate il2 -> cmp_list icmp il1 il2
  | Record vdl1, Record vdl2 -> cmp_list cmp_vardef vdl1 vdl2
  | Array (el1, li1), Array (el2, li2) -> cmp_pair (cmp_lexpr el1 el2) (li_cmp li1 li2)
  | Delta (v1a, v1b), Delta (v2a, v2b) -> cmp_pair (Adavalue.cmp v1a v2a) (Adavalue.cmp v1b v2b)
  | _ -> cmp t1 t2
           
and cmp_vardef d1 d2 =
  cmp_all [ (icmp d1.varname d2.varname) ;
            (cmp d1.const d2.const) ;
            (cmp_type_expr d1.vartype d2.vartype) ;
            (cmp_opt cmp_subt_constraint d1.constrain d2.constrain) ;
            (cmp_opt cmp_expr d1.vinit d2.vinit) ]

and cmp_procdecl pd1 pd2 =
  cmp_all [ (icmp pd1.procname pd2.procname) ;
            (cmp_list cmp_arg pd1.args pd2.args) ;
            (cmp_opt li_cmp pd1.rettype pd2.rettype) ]
    
and cmp_procdef pd1 pd2 =
  cmp_all [ (cmp_procdecl pd1.decl pd2.decl) ;
            (cmp_pl_decl pd1.declarations pd2.declarations) ;
            (cmp_expr pd1.body pd2.body) ;
            (cmp pd1.proc_comments pd2.proc_comments) ]
    
