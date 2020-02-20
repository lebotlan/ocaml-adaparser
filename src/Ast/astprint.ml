open Idents
open Ast
open Parse_errors
open Adavalues

(* Indentation *)
let indent = "   "

let space n x = String.make (n + String.length x) ' '

let idty s = s

(* "2b" or "tob" = "to block" : includes margin, line returns 
 * "2s" or "tos" = "to string": no margin, no line returns *)

(* list + tos => tob *)
let slist2b pre post tos ~margin l = Common.sep (fun x -> margin ^ pre ^ tos x ^ post ^ "\n") "" l

(* list + tob => tob *)
let blist2b tob ~margin l = Common.sep (fun x -> tob ~margin x) "" l

let comments2b ~margin comments = slist2b "-- " "" idty ~margin comments

let pv2s title tob ~margin pv = 
  tob ~margin pv.pv ^

  if pv.errors = [] then ""
  else
    Printf.sprintf "\n%s---- Errors in %s ----\n" margin title ^
    slist2b "--* " "" lp2s ~margin pv.errors ^ "\n"

(* ------------------------------------------------------------------------------------------ *)

let mode2s = function
  | In -> ""
  | Out -> "out "
  | InOut -> "in out "

let clause2b ~margin = function
  | With li -> margin ^ "with " ^ li2s li ^ " ;\n" 
  | Use li  -> margin ^ "use "  ^ li2s li ^ " ;\n"
  | Usetype li -> margin ^ "use type "  ^ li2s li ^ " ;\n"

let packrenames ~margin pr = Printf.sprintf "%spackage %s renames %s ;\n" margin (l2s pr.pack_alias) (li2s pr.pack_orig)

let rec declaration2b ~margin = function
  | Withclause c -> clause2b ~margin c

  | Rename pr -> packrenames ~margin pr

  | Packnew (pname, pfunc, args) -> Printf.sprintf "%spackage %s is new %s(%s) ;\n"
                                      margin (l2s pname) (li2s pfunc) (Common.sep ltype2s ", " args)

  | Typedef (id, args, te, ocons) ->
    let sid = l2s id in
    Printf.sprintf "\n%stype %s%s is %s%s ;\n\n" margin sid (args2s args) (type2s ~margin te) (ocons2s ocons)
                          
  | Subtype (id, lid, ocons) ->
    Printf.sprintf "\n%ssubtype %s is %s%s ;\n\n" margin (l2s id) (li2s lid) (ocons2s ocons)

  | Funrename fr -> Printf.sprintf "\n%s%s renames %s ;\n\n" margin (procdecl2s ~margin fr.fun_alias) (li2s fr.fun_orig)
  | Procdecl decl -> "\n" ^ margin ^ procdecl2s ~margin decl ^ " ;\n\n"
  | Procdef def -> "\n" ^ procdef2b ~margin def ^ "\n"      
  | Vardef vdef -> vardef2b ~margin vdef
  | Package pc -> packcontent2b ~margin pc

and pv_declaration2b title1 title2 ~margin p = pv2s (title1 ^ " " ^ title2) (blist2b declaration2b) ~margin p

and packcontent2b ~margin pc =

  let name = li2s pc.package_name
  and margin2 = indent ^ margin in

  Printf.sprintf "%s\n%spackage%s %s is\n%s\n%send %s ;\n"
    (comments2b ~margin pc.package_comments)
    margin
    (if pc.package_sig then "" else " body") name
    (blist2b (pv_declaration2b "package" name) ~margin:margin2 pc.package_declarations)
  
    (match pc.package_init with
     | None -> ""
     | Some e -> Printf.sprintf "%sbegin\n%s ;\n" margin (expr2s ~margin:margin2 e))
    name

and get_kw_return decl =
  match decl.rettype with
  | None -> "procedure", ""
  | Some li -> "function", " return " ^ li2s li
                               
and procdecl2s ~margin:_ decl =
  let (kw, return) = get_kw_return decl in
  kw ^ " " ^ l2s decl.procname ^ args2s decl.args ^ return

and procdef2b ~margin def =

  let margin2 = margin ^ indent
  and (kw, _) = get_kw_return def.decl in
  
  (comments2b ~margin def.proc_comments) ^
  margin ^ (procdecl2s ~margin:margin2 def.decl) ^ " is\n" ^
  
  (blist2b (pv_declaration2b kw (l2s def.decl.procname)) ~margin:margin2 def.declarations) ^
  
  margin ^ "begin\n" ^
  margin2 ^ expr2s ~margin:margin2 def.body ^ " ;\n" ^
  margin ^ "end " ^ l2s def.decl.procname ^" ;\n"

and vardef2b ~margin vdef =
  let margin2 = margin ^ indent in
  
  Printf.sprintf "%s%s : %s%s%s%s ;\n"
    margin (l2s vdef.varname)
    (if vdef.const then "constant " else "")
    (type2s ~margin:margin2 vdef.vartype)
    (match vdef.constrain with
     | None -> ""
     | Some r -> subconstraint2s r)
    (match vdef.vinit with
     | None -> ""
     | Some e -> " := " ^ expr2s ~margin:margin2 e)

and ltype2s (ol, t) = match ol with
  | None -> li2s t
  | Some l -> Printf.sprintf "%s => %s" (l2s l) (li2s t)
  
and arg2s arg = Printf.sprintf "%s : %s%s%s"
    (l2s arg.argname) (mode2s arg.mode) (li2s arg.argtype)
    (match arg.argdefault with
     | None -> ""
     | Some e -> " := " ^ expr2s ~margin:"" e)

and args2s = function
  | [] -> ""
  | l -> "(" ^ Common.sep arg2s " ; " l ^ ")"

and type2s ~margin = function
  | Abstract -> ""
  | Typename ty -> li2s ty
  | Enumerate l -> "(" ^ Common.sep l2s ", " l ^ ")"
  | Delta (d,g) -> Printf.sprintf "delta %s digits %s" (Adavalue.tos d) (Adavalue.tos g)
    
  | Record fields ->
    let margin' = margin ^ indent in
    "record\n" ^
    Common.sep (fun f -> vardef2b ~margin:margin' f) "" fields ^
    margin ^ "end record"
    
  | Array (rl, ty) ->
    "array (" ^ Common.sep (expr2s ~margin:"")  ", " rl ^ ") of " ^ (li2s ty) 

and ocons2s = function
  | None -> ""
  | Some subc -> subconstraint2s subc

and subconstraint2s = function
  | Index_constraint l -> "(" ^ Common.sep (expr2s ~margin:"")  ", " l ^ ")"
  | Range_constraint r -> " range " ^ expr2s ~margin:""  r

and expr2s ~margin = function
  | Value av -> Adavalue.tos ~margin av
  | Id id -> l2s id
  | Assign (e1, e2) -> Printf.sprintf "%s := %s" (expr2s ~margin:"" e1) (expr2s ~margin:"" e2)
  | Return  e -> Printf.sprintf "return %s" (expr2s ~margin e)

  | Tuple nels -> "(" ^ Common.sep (nexpr2s ~margin:(margin ^ " ")) ", " nels ^ ")"

  (* Infix binary op *)
  | App (Value av, [ ([], e1) ; ([], e2) ]) ->
    expr2s ~margin e1 ^ " " ^ Adavalue.tos ~margin av ^ " " ^ expr2s ~margin e2

  | App (e, nels) ->
    let pre = expr2s ~margin e in
    pre ^ "(" ^ Common.sep (nexpr2s ~margin:(margin ^ space 1 pre)) ", " nels ^ ")"

  | Select (e, l) -> expr2s ~margin e ^ "." ^ (l2s l)
  | Tick (e1, e2) -> expr2s ~margin e1 ^ "'" ^ expr2s ~margin e2

  | If (e1, e2, e3) ->
    let margin' = margin ^ indent in
    "\n" ^ margin ^ "if " ^ expr2s ~margin:"" e1 ^ " then\n" ^
    margin' ^ expr2s ~margin:margin' e2 ^ " ;\n" ^
    margin ^ "else\n" ^
    margin' ^ expr2s ~margin:margin' e3 ^ " ;\n" ^
    margin ^ "end if"

  | Exitwhen e ->
    "exit when " ^ expr2s ~margin:(margin ^ space 10 "") e

  | While (e1, e2) ->
    let margin' = margin ^ indent in
    "\n" ^ margin ^ "while " ^ expr2s ~margin:"" e1 ^ " loop\n" ^
    margin' ^ expr2s ~margin:margin' e2 ^ " ;\n" ^
    margin ^ "end loop"

  | For (iof,rv, id, e1, e) ->

    let kw = match iof with `OF -> " of " | `IN -> " in " in
    
    let margin' = margin ^ indent in
    "\n" ^ margin ^ "for " ^ l2s id ^ kw ^ (if rv then "reverse " else "") ^ expr2s ~margin:"" e1 ^ " loop\n" ^
    margin' ^ expr2s ~margin:margin' e ^ " ;\n" ^
    margin ^ "end loop"
    
  | Declare (decls, e) ->
    let margin2 = margin ^ indent in
    "declare\n" ^
    (blist2b declaration2b ~margin:margin2 decls) ^
    margin ^ "begin\n" ^
    margin2 ^ expr2s ~margin:margin2 e ^
    margin ^ "end declare"
    
  | Case (e, whens) ->
    let margin' = margin ^ indent in
    "case " ^ expr2s ~margin:"" e ^ " is\n" ^
    blist2b when2b ~margin:margin' whens ^
    margin ^ "end case"

  | Seq l -> Common.sep (expr2s ~margin) (" ;\n" ^ margin) l

  | New (id, []) -> "new " ^ li2s id
  | New (id, l) -> "new " ^ li2s id ^ "'(" ^ Common.sep (expr2s ~margin:"") ", " l ^ ")"

  | Is_in (e, range) -> expr2s ~margin e ^ " in " ^ expr2s ~margin:""  range

  | Try (e, whens) ->
    let margin' = margin ^ indent in
    expr2s ~margin e ^ "\n" ^ margin ^ "exception\n" ^
    blist2b when2b ~margin:margin' whens

  | Unconstrained -> "<>"
  | Interval (e1, e2) -> expr2s ~margin:"" e1 ^ ".." ^ expr2s ~margin:"" e2
  | TickRange (e,io) -> expr2s ~margin:"" e ^ "'range" ^ (match io with None -> "" | Some i -> "(" ^ Adavalue.tos i ^ ")")
  | Range (e1, e2) -> expr2s ~margin:"" e1 ^ " range " ^ expr2s ~margin:"" e2

and when2b ~margin = function
  | Match (io, l, e) ->
    let margin' = margin ^ indent in
    margin ^ "when " ^ (match io with None -> "" | Some i -> l2s i ^ ": ") ^
    Common.sep (fun av -> expr2s ~margin:margin' av) " | " l ^ " =>\n" ^
    margin' ^ expr2s ~margin:margin' e ^ "\n"

and nexpr2s ~margin (ol, e) = match ol with
  | [] -> expr2s ~margin e
  | l ->
    let labels = labels2s l in
    Printf.sprintf "%s => %s" labels (expr2s ~margin:(margin ^ space 4 labels) e)

and labels2s l = Common.sep (expr2s ~margin:"") " | " l


let file2s ~margin:_ file =

  blist2b (pv_declaration2b "file" file.path) ~margin:"" file.content ^
  
  comments2b ~margin:"" file.file_comments ^
  "\n\n"

let pfile2s pfile =
  let title = Printf.sprintf "---- File %s ----\n\n" pfile.pv.path in
  pv2s title file2s ~margin:"" pfile


let procdecl2s decl = procdecl2s ~margin:"" decl
    
