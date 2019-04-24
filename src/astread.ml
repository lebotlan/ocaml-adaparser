open Astmap
open Ast
open Parse_errors
open Namespace
open Loc

(*** All errors ***)

let rec all_errors_declaration = function
  | Withclause _
  | Rename _
  | Packnew _ -> punit
      
  | Package pc -> all_errors_package_content pc
                    
  | Typedef (_, argl, texpr, osubt) ->
    unitjoin all_errors_arg argl >>>
    all_errors_type_expr texpr >>>
    Common.option_apply punit osubt all_errors_subt_constraint
      
  | Subtype (_, _, osubt) -> Common.option_apply punit osubt all_errors_subt_constraint                               
  | Procdef def -> all_errors_procdef def
  | Procdecl decl -> all_errors_procdecl decl
  | Funrename fr -> all_errors_procdecl fr.fun_alias                      
  | Vardef vdef -> all_errors_vardef vdef

and all_errors_oexpr oe = Common.option_apply punit oe all_errors_expr

and all_errors_pv_declaration dlp = dlp >>= (fun dl -> unitjoin all_errors_declaration dl)

and all_errors_package_content pc =
  unitjoin all_errors_pv_declaration pc.package_declarations >>>
  all_errors_oexpr pc.package_init

and all_errors_arg arg = all_errors_oexpr arg.argdefault

and all_errors_type_expr = function
  | Abstract 
  | Typename _
  | Enumerate _
  | Delta (_,_) -> punit
    
  | Record vdl -> unitjoin all_errors_vardef vdl
  | Array (el, _) -> unitjoin all_errors_expr el

and all_errors_subt_constraint = function
  | Index_constraint el -> unitjoin all_errors_expr el
  | Range_constraint e -> all_errors_expr e

and all_errors_procdecl decl = unitjoin all_errors_arg decl.args

and all_errors_procdef def =
  all_errors_procdecl def.decl >>>
  unitjoin all_errors_pv_declaration def.declarations >>>
  all_errors_expr def.body

and all_errors_vardef vdef =
  all_errors_type_expr vdef.vartype >>>
  Common.option_apply punit vdef.constrain all_errors_subt_constraint >>>
  all_errors_oexpr vdef.vinit

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
  | For (_,_,_, e1, e2) -> all_errors_expr e1 >>> all_errors_expr e2      
  | Declare (decls, e) -> unitjoin all_errors_declaration decls >>> all_errors_expr e
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

and all_errors_nexpr (_, e) = all_errors_expr e

and all_errors_when = function
  | Match (_, el, e) -> unitjoin all_errors_expr (e :: el)

and all_errors_nexprlist l = unitjoin all_errors_nexpr l


let all_errors_file file = unitjoin all_errors_pv_declaration file.content
  
let all_errors fpv = fpv >>= all_errors_file



(*** All procdecl ***)
type acu =
  { allp: (namespace * procdecl) list ;
    nmsp: namespace }

let init_acu =
  { allp = [] ;
    nmsp = init_namespace }

let insert_decl decl acu =
  { acu with allp = (acu.nmsp, decl) :: acu.allp }

let all_procdecl_map =
  object
    inherit [acu] tree_mapper

    (* up: reset namespace only. *)
    method! upacu inacu outacu = { allp = outacu.allp ;
                                   nmsp = inacu.nmsp }
        
    method! use_id li acu = return li { acu with nmsp = insert_use li acu.nmsp }
    method! pack_rename pr acu = return pr { acu with nmsp = insert_pr pr acu.nmsp }
    method! procdecl decl acu = return decl (insert_decl decl acu)
  end

let all_procdecl fpv = (all_procdecl_map#content fpv.pv.content init_acu).racu.allp


(*** All with/use clauses ***)
let all_wclauses_map =
  object
    inherit [withclause list] tree_mapper

    method! upacu _ outacu = outacu
    method! withclause wc acu = return wc (wc :: acu)
  end

let all_wclauses fpv = (all_wclauses_map#content fpv.pv.content []).racu


(*** Declarations in a package spec. *)
let package_decls f =

  let rec loop = function
    (* The package content could not be found (package Foo is ...) *)
    | [] -> pv ~err:{ pos = f.fpos ; v = Missing "package content" } []

    | { pv = [] ; _ } :: rest -> loop rest

    (* We have found a package declaration *)
    | { pv = Package pc :: _ ; _ } :: _ -> pv (List.flatten (List.map (fun pvd -> pvd.pv) pc.package_declarations))

    (* Ignore other top-level declarations *)
    | { pv = _ :: rest1 ; errors } :: rest2 -> loop ( { pv = rest1 ; errors } :: rest2 )

  in

  loop f.content
