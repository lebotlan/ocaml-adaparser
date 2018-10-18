open Idents
open Ast
open Parse_errors
open Adavalues

(* Indentation *)
let indent = "   "

let space n x = String.make (n + String.length x) ' '

let clause2s = function
  | With li -> "with " ^ li2s li ^ " ;\n" 
  | Use li  -> "use "  ^ li2s li ^ " ;\n"

let mode2s = function
  | In -> ""
  | Out -> "out "
  | InOut -> "in out "

let rec arg2s arg = Printf.sprintf "%s : %s%s%s"
    (l2s arg.argname) (mode2s arg.mode) (li2s arg.argtype)
    (match arg.argdefault with
     | None -> ""
     | Some e -> " := " ^ expr2s ~margin:"" e)

and args2s = function
  | [] -> ""
  | l -> "(" ^ Common.sep arg2s " ; " l ^ ")"

and expr2s ~margin = function
  | Value av -> Adavalue.tos ~margin av
  | Id id -> li2s id
  | Assign (e1, e2) -> Printf.sprintf "%s := %s" (expr2s ~margin:"" e1) (expr2s ~margin:"" e2)
  | Return  e -> Printf.sprintf "return %s" (expr2s ~margin e)

  | Tuple nels -> "(" ^ Common.sep (nexpr2s ~margin:(margin ^ " ")) ", " nels ^ ")"

  (* Infix binary op *)
  | App (Value av, [ (None, e1) ; (None, e2) ]) ->
    expr2s ~margin e1 ^ " " ^ Adavalue.tos ~margin av ^ " " ^ expr2s ~margin e2

  | App (e, nels) ->
    let pre = expr2s ~margin e in
    pre ^ "(" ^ Common.sep (nexpr2s ~margin:(margin ^ space 1 pre)) ", " nels ^ ")"

  | Select (e, l) -> expr2s ~margin e ^ "." ^ (l2s l)
  | Tick (e, l) -> expr2s ~margin e ^ "'" ^ (l2s l)

  | If (e1, e2, e3) ->
    let margin' = margin ^ indent in
    "\n" ^ margin ^ "if " ^ expr2s ~margin:"" e1 ^ " then\n" ^
    margin' ^ expr2s ~margin:margin' e2 ^ " ;\n" ^
    margin ^ "else\n" ^
    margin' ^ expr2s ~margin:margin' e3 ^ " ;\n" ^
    margin ^ "end if"
    
  | While (e1, e2) ->
    let margin' = margin ^ indent in
    "\n" ^ margin ^ "while " ^ expr2s ~margin:"" e1 ^ " loop\n" ^
    margin' ^ expr2s ~margin:margin' e2 ^ " ;\n" ^
    margin ^ "end loop"

  | For (id, range, e) ->
    let margin' = margin ^ indent in
    "\n" ^ margin ^ "for " ^ l2s id ^ " in " ^ expr2s ~margin:"" range ^ " loop\n" ^
    margin' ^ expr2s ~margin:margin' e ^ " ;\n" ^
    margin ^ "end loop"
    
  | Declare (decls, e) ->
    let margin' = margin ^ indent in
    "declare\n" ^
    margin' ^ (Common.sep (decl2s ~decl_only:false ~margin:margin') ("\n" ^ margin') decls) ^
    margin ^ "begin\n" ^
    margin' ^ expr2s ~margin:margin' e ^
    margin ^ "end declare"
    
  | Case (e, whens) ->
    let margin' = margin ^ indent in
    "case " ^ expr2s ~margin:"" e ^ " is\n" ^
    margin' ^ (Common.sep (fun w -> when2s ~margin:margin' w ^ " ;\n") margin' whens) ^
    margin ^ "end case"

  | Seq l -> Common.sep (expr2s ~margin) (" ;\n" ^ margin) l

  | New (id, None) -> "new " ^ li2s id
  | New (id, Some e) -> "new " ^ li2s id ^ "'(" ^ expr2s ~margin:"" e ^ ")"

  | Is_in (e, range) -> expr2s ~margin e ^ " in " ^ expr2s ~margin:""  range

  | Try (e, whens) ->
    let margin' = margin ^ indent in
    expr2s ~margin e ^ "\n" ^ margin ^ "exception\n" ^
    margin' ^ (Common.sep (fun w -> when2s ~margin:margin' w ^ " ;\n") margin' whens)

  | Unconstrained -> "<>"
  | Interval (e1, e2) -> expr2s ~margin:"" e1 ^ ".." ^ expr2s ~margin:"" e2
  | TickRange (e,io) -> expr2s ~margin:"" e ^ "'range" ^ (match io with None -> "" | Some i -> "(" ^ Adavalue.tos i ^ ")")
  | Range (e1, e2) -> expr2s ~margin:"" e1 ^ " range " ^ expr2s ~margin:"" e2


and when2s ~margin = function
  | Others e -> "when others => " ^ expr2s ~margin:(margin ^ space 11 "") e
  | Match (l, e) ->
    let margin' = margin ^ indent in
    "when " ^ Common.sep (fun av -> expr2s ~margin:margin' av) " | " l ^ " =>\n" ^
    margin' ^ expr2s ~margin:margin' e

and nexpr2s ~margin (ol, e) = match ol with
  | None -> expr2s ~margin e
  | Some l ->
    let label = l2s l in
    Printf.sprintf "%s => %s" label (expr2s ~margin:(margin ^ space 4 label) e)

and ltype2s (ol, t) = match ol with
  | None -> li2s t
  | Some l -> Printf.sprintf "%s => %s" (l2s l) (li2s t)

and comment2s ~margin comments =
  margin ^ (Common.sep (fun s -> "-- " ^ s) ("\n" ^ margin) comments) ^ "\n"

and decl2s: 'a . decl_only:bool -> margin:string -> 'a declaration -> string =
  fun ~decl_only ~margin -> function
  | Procdef pdef -> procdef2s ~decl_only ~margin pdef ^ (if decl_only then " ;\n" else "")
                     
  | Rename pr -> Printf.sprintf "%spackage %s renames %s ;" margin (l2s pr.pack_alias) (li2s pr.pack_orig)

  | Packnew (pname, pfunc, args) -> Printf.sprintf "%spackage %s is new %s(%s) ;"
                                      margin (l2s pname) (li2s pfunc) (Common.sep ltype2s ", " args)

  | Funrename fr -> Printf.sprintf "%s%s renames %s ;" margin (procdef2s ~decl_only:true ~margin fr.fun_alias) (li2s fr.fun_orig)
                  
  | Typedef (id, te) ->
    let sid = l2s id in
    "\n" ^ margin ^ "type " ^ sid ^ " is " ^ type2s ~margin:margin te ^ " ;\n"
                          
  | Subtype (id, lid, ocons) ->
    margin ^ "subtype " ^ l2s id ^ " is " ^ li2s lid ^
    (match ocons with
     | None -> ""
     | Some subc -> subconstraint2s subc) ^ " ;\n"

  | Vardef vdef -> Printf.sprintf "%s%s : %s%s%s%s ;"
                     margin (l2s vdef.varname)
                     (if vdef.const then "constant " else "")
                     (li2s vdef.vartype)
                     (match vdef.varrange with
                      | None -> ""
                      | Some r -> "(" ^ Common.sep (expr2s ~margin:"")  ", " r ^ ")")
                     (match vdef.vinit with
                      | None -> ""
                      | Some e -> " := " ^ expr2s ~margin:" " e)

  | Withclause c -> clause2s c
                     
and type2s ~margin = function
  | Enumerate l -> "(" ^ Common.sep l2s ", " l ^ ")"
  | Delta (d,g) -> Printf.sprintf "delta %s digits %s" (Adavalue.tos d) (Adavalue.tos g)
    
  | Record fields ->
    let margin' = margin ^ indent in
    "record\n" ^
    Common.sep (fun f -> margin' ^ field2s f ^ "\n") "" fields ^
    margin ^ "end record"
    
  | Array (rl, ty) ->
    "array (" ^ Common.sep (expr2s ~margin:"")  ", " rl ^ ") of " ^ (li2s ty) 

and field2s fi = Printf.sprintf "%s : %s%s ;"
    (l2s fi.fname) (li2s fi.ftype)
    (match fi.fsub with
     | None -> ""
     | Some subt -> subconstraint2s subt)

and subconstraint2s = function
  | Index_constraint l -> "(" ^ Common.sep (expr2s ~margin:"")  ", " l ^ ")"
  | Range_constraint r -> " range " ^ expr2s ~margin:""  r

and procdef2s: 'a . decl_only:bool -> margin:string -> 'a procdef -> string =
  fun ~decl_only ~margin pdef ->
  let (kw, return) = match pdef.rettype with
    | None -> "procedure", ""
    | Some li -> "function", " return " ^ li2s li
  in

  let margin' = margin ^ indent in

  "\n" ^
  (if pdef.proccomments = [] then "" else comment2s ~margin pdef.proccomments ^ "\n") ^
  margin ^ kw ^ " " ^ l2s pdef.procname ^ args2s pdef.args ^ return ^
  (if decl_only then ""
   else
     (" is\n\n" ^
      Common.sep (fun d -> decl2s ~decl_only:false ~margin:margin' d ^ "\n") "" pdef.declarations ^
      margin ^ "begin\n" ^
      margin' ^ expr2s ~margin:margin' pdef.body ^ " ;\n" ^
      margin ^ "end " ^ l2s pdef.procname ^" ;\n" ^
      (if pdef.sub_errors.errors = [] then "\n"
       else
         Printf.sprintf "%s+-------- Errors in %s ------------------------------------\n%s|\n"
           margin (l2s pdef.procname) margin ^
         lperr2s ~margin:(margin ^ "|   ") pdef.sub_errors.errors ^
         "\n" ^ margin ^ "+--------------------------------------------------------------\n\n")
  ))
  

let package2s ~decl_only pack =
  let name = li2s pack.package_name in
  Printf.sprintf "%s\npackage%s %s is\n%s\n%send %s ;\n"
    (comment2s ~margin:"" pack.package_comments)
    (if decl_only then "" else " body") name
    (Common.sep (decl2s ~decl_only ~margin:indent) "\n" pack.package_declarations)
    (match pack.package_init with None -> "" | Some e -> "begin\n" ^ expr2s ~margin:indent e ^ " ;\n")
    name

let compilation_unit2s = function
  | Program pdef -> procdef2s ~decl_only:false ~margin:"" pdef
  | Package_Sig pack -> package2s ~decl_only:true pack
  | Package_Body pack -> package2s ~decl_only:false pack
  | No_cu -> "\n[[ No compilation unit ]]\n"

let ast2s ast =
  Common.sep clause2s "\n" ast.clauses ^ "\n" ^
  (compilation_unit2s ast.c_unit)

let file2s file =
  Printf.sprintf "---- File %s ----\n\n" file.file ^
  ast2s file.ast.pv ^ "\n\n" ^
  if file.ast.errors = [] then ""
  else
    begin
      Printf.sprintf "---- Errors in file %s ----\n" file.file ^
      lperr2s ~margin:"" file.ast.errors ^ "\n\n"
    end


