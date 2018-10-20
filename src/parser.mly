
(*
 * This is a parser for something that looks like a subset of Ada 2005.
 * This file must be compiled with menhir (a high-level parser generator for ocaml). 
 *
 * Note : 
 *   - for the application I had in mind, this parser (and lexer) read only valid Ada files,
 *     hence we take some liberty with respect to the Ada spec.
 *)

%{
open Astlib
open Ast
open Loc
open Adavalues
open Adabuiltins
open Parse_errors
open Idents
open Builtins


let vun = Value Adavalue.unit

(* long: long, qualified identifier *)
let longtos l = Common.sep i2s "." l

let expected ?(may_be_empty=false) expected_string read_ident =
  if may_be_empty && is_empty read_ident then ()
  else
    if Idents.equal expected_string read_ident then ()
    else syntax_error read_ident.pos (Mismatch (i2s expected_string.v, Idents.i2s read_ident.v))
                        
let expected_long exp_longname read_longname =
  if Idents.long_equal exp_longname read_longname then ()
  else syntax_error read_longname.pos (Mismatch (longtos exp_longname.v, longtos read_longname.v))

(* let integer = Idents.norm "integer" *)

let get_num num =
  let (x, real) = num.v in
      
  (* int_of_string and float_of_string should not fail (except if bases other than 10 are used. *)
  if real then
    begin
      try Adavalue.mk_float (float_of_string x)
      with _ -> assert false
    end
  else 
    begin 
      try Adavalue.mk_int (int_of_string x) 
      with _ ->
        if String.contains x '#' then failwith ("Cannot handle non decimal digits: " ^ x)
        else assert false
    end

    (* let get_int num = Adavalue.get_int (get_num num) *)
                      
%}

(* Keywords *)
%token ABS ABSTRACT ACCEPT ALL
%token ACCESS ALIASED AND 
%token ARRAY BEGIN BODY 
%token CASE 
%token CONSTANT DECLARE DELAY DELTA DIGITS
%token DO ELSE ELSIF 
%token ENTRY EXCEPTION EXIT 
%token FOR FUNCTION GENERIC GOTO 
%token IF IN IS LIMITED 
%token LOOP MOD NEW NOT 
%token NULL OF OR OTHERS 
%token OUT PACKAGE PRAGMA PRIVATE 
%token PROCEDURE PROTECTED RAISE RANGE 
%token RECORD REM RENAMES REQUEUE 
%token RETURN REVERSE SELECT SEPARATE 
%token SUBTYPE TAGGED TASK TERMINATE 
%token THEN TYPE UNTIL USE 
%token WHEN WHILE WITH XOR 

(* END and EOF are special: they contain the list of comments read since the previous END or beginning of file. *)
%token <string list> END
%token <string list> EOF
                                            
(* Delimiters *)
%token DOT LT LPAREN PLUS 
%token BAR AMPAND STAR RPAREN  
%token SEMI MINUS SLASH COMMA 
%token GT COLON EQUAL TICK TICKRANGE
%token DOTDOT LTLT BRAKET LEQ 
%token STARSTAR NOTEQ GTGT GEQ 
%token ASSIGN IMPLY 

(* Identifier *)
%token <Astlib.Idents.s> IDENT

(* Numeric literal, bool indicates if it contains a point. *)
%token <string * bool> NUM

(* Character or string literal *)
%token <char>   CHAR
%token <string> STRING
                                                                                                                      
%start <Astlib.Ast.ast Astlib.Parse_errors.pv> file

%start <unit> decl_file

       
(* Precedence *)

%left ACCESS
%left TICK
      
                                                 
%left AMPAND

%left OR
%left AND

%nonassoc IN THEN RANGE ELSE
      
%nonassoc LEQ GEQ LT GT EQUAL NOTEQ

%nonassoc NOT

%left MOD REM         
%left PLUS MINUS    
%left STAR SLASH STARSTAR
              
%%

(*********************************************************************)
(*                     PARTIAL ADA GRAMMAR                           *)
(*                                                                   *)
(*          This is a very ad-hoc subset of Ada 2005.                *)
(*********************************************************************)

(*********************  Useful bindings  *********************)

(* Lifts X into the error monad. *)
p_v(X): x=X { pv x }

%inline pars(X): LPAREN x=X RPAREN { x }
            
(* Parenthesized list *)
parlist(ELT, SEP): args = pars(separated_nonempty_list(SEP, ELT)) { args }
                            
(* Comma-list *)
comalist(ELT): elts = separated_nonempty_list(COMMA, ELT) { elts }

p_separated_nonempty_list(SEP, ELT):
| e=ELT { e >>:: pnil }
| e=ELT SEP l=p_separated_nonempty_list(SEP, ELT) { e >>:: l }
                        
p_comalist(ELT): elts=p_separated_nonempty_list(COMMA, ELT) { elts }

p_nonempty_list(ELT):
| e=ELT { e >>:: pnil }
| e=ELT l=p_nonempty_list(ELT) { e >>:: l }

p_list(ELT):
| empty { pv [] }
| l=p_nonempty_list(ELT) { l }

p_flatlist(ELT): l=p_list(ELT) { l >>= (fun l -> pv (List.flatten l)) }
      
(* Localized identifier *)
loc_ident:
| id=IDENT { mkloc $loc id }
| ALL      { mkloc $loc all }
      
(* Long name with dots only, e.g. Ada.Text_IO ou Package.varname *)
p_dotted_name: ids = p_separated_nonempty_list(DOT, p_v(IDENT)) { swloc (mkloc $loc ids) }

dotted_name: ids = separated_nonempty_list(DOT, IDENT) { mkloc $loc ids } 


(****************  FILES, COMPILATION UNITS  *****************)

file: 
(* A file must contain only one compilation unit, preceded by a list of clauses ('with' and 'use') *)
| cl=p_flatlist(clause) cu=compilation_unit g=garbage? EOF
                      {
                        (* Trailing garbage is recorded as an error. *)
                        let err = match g with
                          | None -> None
                          | Some l -> Some (mkloc $loc(g) (Ignored ("Unexpected text after main program.", List.length l)))
                        in

                        (* All clauses are flattened. *)
                        cl >>= (fun clauses -> cu >>=
                                                 (fun c_unit -> pv ?err { clauses ;
                                                                          c_unit } ))
                      }

(* IDENTs found at the beginning of the file are ignored (and recorded as errors). *)                      
| _id=IDENT pf=file { pf >>= (fun f -> pv ~err:(mkloc $loc(_id) (Ignored ("Ident", 1))) f) }

(* 'with' or 'use' clause *)
(* SPEC: clause is called 'context_item' *)
clause: 
| WITH names=p_comalist(p_dotted_name) SEMI { p_map names (fun n -> pv (With n)) }
| USE  names=p_comalist(p_dotted_name) SEMI { p_map names (fun n -> pv (Use n)) }

(* A compilation unit is either a program, a package signature, or a package body. *)
compilation_unit:
| p = program                     { p >>= (fun p -> pv (Program p)) }
| PRIVATE? p = package_sig        { p >>= (fun p -> pv (Package_Sig p)) }

(* Not sure private can occur here, but it removes yet another shift/reduce conflict. *)
| PRIVATE? p = package_body       { p >>= (fun p -> pv (Package_Body p)) }
| generic                         { assert false }

init_block: BEGIN b=block         { b }
                       
empty: { [] }


(********************  PACKAGES  *********************)
        
package_sig: 
PACKAGE package_name=dotted_name IS 
        pdecl=p_flatlist(declaration(proc_and_fun_decl))
        package_comments=END endname=dotted_name SEMI
                                                                                     
			 { expected_long package_name endname ;
			   pdecl >>= (fun package_declarations ->
				      pv { package_name ; package_declarations ; package_comments ; package_init = None })
                         }
                                          
package_body:
    PACKAGE BODY package_name=dotted_name IS
        pdecl=definitions
        b=init_block?
        package_comments=END endname=dotted_name SEMI
                                          
			 { expected_long package_name endname ;
			   pdecl >>= (fun package_declarations ->
                             (swopt b) >>= (fun b ->
			                pv { package_name ; package_declarations ; package_comments ; package_init = b }))
			 }

(****************  PROCEDURES AND FUNCTIONS  *****************)

definitions: d=p_flatlist(declaration(proc_and_fun_def)) { d }
                         
(* A simple program *)
program: p=proceduredef(empty) { pv p }
                                  
proceduredecl(ARGS):
       PROCEDURE procname=loc_ident args=ARGS
                   { { procname ; args ; declarations=[] ; body=vun ; proccomments=[] ; rettype = None ; sub_errors = pv () } : decl_only procdef }

(* A procedure definition *)
proceduredef(ARGS):
       pdecl = proceduredecl(ARGS) IS
           declarations=definitions
       BEGIN
           body=exn_block?              
       coms=END endname=loc_ident? SEMI
							
                  { expected ~may_be_empty:true pdecl.procname (Common.option_default endname empty_ident) ; 
		    let body =
		      match body with
		      | None -> pv ~err:(mkloc $loc(body) (Missing "body")) vun
		      | Some b -> b
		    in

		    let sub_errors =
		      body >>= (fun _ -> declarations >>= (fun _ -> pv ()))
		    in
		    
                    { pdecl with
		      declarations = declarations.pv ;
		      body = body.pv ;
		      proccomments = coms ;
		      sub_errors } : def procdef }
(*
(* Empty procedure *)                  
|     pdecl = proceduredecl(ARGS) IS?
      coms=END endname=loc_ident SEMI
                   {
                     expected pdecl.procname endname ; 
                     let sub_errors =
                       pv ~err:(mkloc $loc (Empty ("procedure " ^ i2s pdecl.procname.v))) ()
                     in
                     ( { pdecl with
                         proccomments = coms ;
                         sub_errors } : def procdef) }
 *)                   
functiondecl:
       FUNCTION procname=loc_ident args=argsdef RETURN ret=typename
                   { { procname ; args ; declarations=[] ; body=vun ; proccomments=[] ; rettype = Some ret ; sub_errors = pv () } : decl_only procdef }
                                  
functiondef:
       fdecl = functiondecl IS
           decl=definitions
       BEGIN
         body=exn_block
       coms=END endname=loc_ident? SEMI
						   
                { expected ~may_be_empty:true fdecl.procname (Common.option_default endname empty_ident) ; 
                  
		  let sub_errors =
		    body >>= (fun _ -> decl >>= (fun _ -> pv ()))
		  in
		  { fdecl with
		    declarations = decl.pv ;
		    body = body.pv ;
		    proccomments = coms ;
		    sub_errors } : def procdef }

|     fdecl = functiondecl IS?
      coms=END endname=loc_ident SEMI
               {
                 expected fdecl.procname endname ;
                 let sub_errors =
                   pv ~err:(mkloc $loc (Empty ("function " ^ i2s fdecl.procname.v))) ()
                 in
                 { fdecl with
                   proccomments = coms ;
                   sub_errors } }
                
(* Arguments *)
argsdef: l=loption(parlist(arg, SEMI)) { List.flatten l }
                                                        
arg: names=comalist(loc_ident) COLON mode=mode? argtype=typename argdefault=argdefault?
                                  { List.map (fun argname -> { argname ;
                                                               argtype ;
                                                               mode = Common.option_default mode In ;
                                                               argdefault })
                                      names }

argdefault: ASSIGN e=expr { e }
                                  
mode: 
| IN { In }
| OUT { Out }
| IN OUT { InOut }

    
vardef:
(* Constant or variable declaration *)
| names=comalist(loc_ident) COLON ALIASED? cst=CONSTANT?
   vt=vartype? i=initialize? SEMI
		   {
                     let const = cst=Some () in
                     
                     match vt with
                     | None ->
                        pv (List.map
                              (fun v -> { varname = v ;
                                          const ;
                                          vartype = Typename empty_long_ident ;
                                          constrain = None ;
                                          vinit = i })
                              names)
                        
                     | Some (t,c) ->
                        t >>= (fun t ->
                         pv (List.map
			       (fun v -> { varname = v ;
					   const ;
					   vartype = t ;
                                           constrain = c ;
					   vinit = i })
			       names)) }

vartype: t=type_expr c=subt_constraint?    { (t, c) }
                                              
                                               
(**************  DECLARATIONS AND DEFINITIONS  ***************)
declaration(proc_and_fun):

(* Private is ignored *)                                              
| PRIVATE        { pv [] }
                                              
(* Package renaming *)
| PACKAGE i=loc_ident RENAMES p=dotted_name SEMI  { pv [ Rename { pack_alias = i ;
								  pack_orig = p }]}

| PACKAGE i=loc_ident IS NEW o=dotted_name l=parlist(ltype, COMMA) SEMI
                                               { pv [ Packnew (i, o, l) ] }

                                            
(* Function renaming *)
| p=proceduredecl(argsdef) RENAMES i=dotted_name SEMI { pv [ Funrename { fun_alias = p ;
                                                                         fun_orig = i }]}
| f=functiondecl RENAMES i=dotted_name SEMI           { pv  [ Funrename { fun_alias = f ;
                                                                          fun_orig = i }]}
| c = clause                                          { p_map c (fun x -> pv (Withclause x)) }
                                       
(* Constant or variable, but type is missing *)							    
| v=loc_ident i=initialize? SEMI   { pv ~err:(mkloc $loc (Missing "type"))
				       [ Vardef { varname = v ;
						  const = false ;
						  vartype = Typename empty_long_ident ;
                                                  constrain = None ;
						  vinit = i }]}

| vdef=vardef { p_map vdef (fun d -> pv (Vardef d)) }

(* Procedure or function declaration or definition *)
| z=proc_and_fun                                                { z >>= (fun d -> pv [Procdef d]) }

| TYPE l=loc_ident SEMI                                         { pv [ Typedef (l, [], Abstract, None) ] }
| TYPE l=loc_ident args=argsdef IS
         NEW? t=type_expr withprivate? r=subt_constraint? SEMI  { t >>= (fun t -> pv [ Typedef (l, args, t, r) ]) }

| TYPE l=loc_ident args=argsdef IS RANGE e=expr SEMI            { pv [Typedef (l, args, Typename empty_long_ident, Some (Range_constraint e)) ] }
                                                        
| SUBTYPE l=loc_ident IS p=dotted_name r=subt_constraint? SEMI  { pv [ Subtype (l, p, r) ]}

| generic { assert false }

(* Ignored *)
withprivate: WITH PRIVATE { }

typename:
| t=dotted_name                    { t }
(*| ALIASED t=dotted_name                    { t } *)
| ACCESS ALL? t=typename           { { pos = t.pos ; v = access :: t.v } }
| t=typename TICK i=loc_ident      { mkloc $loc (t.v @ [i.v]) }
| EXCEPTION                        { mkloc $loc [i_exception] }
                      
ltype:
| l=loc_ident IMPLY t=typename         { (Some l, t) }
| t=typename                           { (None, t) }                        

             
(* Spec : subtype_constraint is a range_constraint, digits_constraint, delta_constraint, index_constraint, or discriminant_constraint. *)
(* We ignore digits_constraint, delta_constraint, discriminant_constraint *)
subt_constraint:
| l=parlist(expr, COMMA)                                 { Index_constraint l }
| RANGE r=expr                                           { Range_constraint r }
            
initialize: ASSIGN e=expr { e }

(* Procedure or function definition or declaration *)
proc_and_fun_def:
| p=proceduredef(argsdef)                                      { pv (p:def procdef) }
| f=functiondef                                                { pv f }

(* Open and rebuild record to force retyping of phantom type. *)                                                               
| d=proc_and_fun_decl                                          { d >>= (fun d -> pv ( { d with procname = d.procname }:def procdef)) }
      
(* Procedure or function declaration *)
proc_and_fun_decl:
| p=proceduredecl(argsdef) SEMI                                { pv p }
| f=functiondecl SEMI                                          { pv f }
      
(************************  TYPEDEFS  *************************)
type_expr:
| TAGGED? LIMITED? PRIVATE                             { pv (Abstract) }
| t=typename                                           { pv (Typename t) }
| l=parlist(loc_ident, COMMA)                          { pv (Enumerate l) }
| RECORD a=vardef* END RECORD                          { swlist a >>= (fun l -> pv (Record (List.flatten l))) }
| ARRAY ranges=parlist(expr,COMMA) OF t=typename       { pv (Array (ranges, t)) }
| DELTA x=pnum DIGITS y=pnum                           { pv (Delta (get_num x, get_num y)) }

(***********************  STATEMENTS  ************************)

block: l=p_nonempty_list(statement)                    { l >>= (fun l -> pv (Seq l)) }

exn_block: b=block e=exn_handler?                      { match e with None -> b
                                                                    | Some l -> l >>= (fun l ->
                                                                        b >>= (fun b -> pv (Try (b,l)))) }

exn_handler:
| EXCEPTION l=p_nonempty_list(when_clause)   { l }
                       
statement: 
| NULL SEMI                                                        { pv vun }
| DELAY e=expr SEMI                                                { pv (App (Value Builtins.delay, [([], e)])) }
| RAISE e=expr SEMI                                                { pv (App (Value Builtins.araise, [([], e)])) }
| GOTO e=expr SEMI                                                 { pv (App (Value Builtins.goto, [([], e)])) }
| EXIT SEMI                                                        { pv (Id (mkloc $loc [i_exit])) }
              
| e=expr SEMI                                                      { pv e }                 

| e1=expr ASSIGN e2=expr SEMI                                      { pv (Assign (e1, e2)) }

| IF e1=expr THEN s1=block s2=elsif END IF SEMI                    { s1 >>= (fun s1 -> s2 >>= (fun s2 -> pv (If (e1, s1, s2)))) }

| FOR l=loc_ident IN r=expr LOOP s=block END LOOP SEMI             { s >>= (fun s -> pv (For (l, r, s))) }
| WHILE e=expr LOOP s=block END LOOP SEMI                          { s >>= (fun s -> pv (While (e, s))) }

| LOOP s=block END LOOP SEMI                                       { s >>= (fun s -> pv (While (Id (mkloc $loc [i_exit]), s))) }

| DECLARE d=definitions BEGIN s=exn_block END SEMI
  	                          { d >>= (fun d -> s >>= (fun s -> pv (Declare (d, s)))) }

| EXIT WHEN e=expr SEMI                                            { pv (Exitwhen e) }

| CASE e=expr IS l=p_nonempty_list(when_clause) END CASE SEMI      { l >>= (fun l -> pv (Case (e, l))) }

| RETURN e=expr SEMI                                               { pv (Return e) }

elsif:
| ELSIF e1=expr THEN s1=block s2=elsif                             { s1 >>= (fun s1 -> s2 >>= (fun s2 -> pv (If (e1, s1, s2)))) }
| ELSE s2=block                                                    { s2 }
| empty                                                            { pv vun }

when_clause: 
| WHEN l=separated_nonempty_list(BAR, expr) IMPLY s=block      { s >>= (fun s -> pv (Match (l, s))) }
                                                        
(* Others is an identifier *)                                                        
(* | WHEN OTHERS IMPLY s=block                                    { s >>= (fun s -> pv (Others s)) } *)

(***********************  EXPRESSIONS  ************************)

(* Expressions which may be followed by a DOT or PARENTHESIS *)
dot_expr:
| v=adavalue                                { Value v }					    
| l=loc_ident                               { Id (mkloc $loc [l.v]) }
| e=dot_expr a=parlist(nexpr, COMMA)        { App (e, a) }
| e=dot_expr DOT l=loc_ident                { Select (e, l) }
| e=dot_expr TICK i=loc_ident               { Tick (e,i) }
| e=dot_expr TICK _a=ACCESS                 { Tick (e,mkloc $loc(_a) access) }
| l=pars(comalist(nexpr))                   { Tuple l }
| NEW n=dotted_name l=tickinit              { New (n, l) }
| NEW n=dotted_name a=parlist(expr, COMMA)  { New (n, a) }
                        
expr:
| e=dot_expr                                { e }
| NEW l=dotted_name                         { New (l, []) }
| e1=expr op=INFIX_OP e2=expr               { App (Value op, [ ([], e1) ; ([], e2) ]) }
| op=PREFIX_OP e=expr                       { App (Value op, [ ([], e) ]) }
| e=expr IN r=expr                          { Is_in (e, r) }
| e=expr NOT IN r=expr                      { App (Value bnot, [ ([], Is_in (e, r)) ]) }

(* Range expressions *)                                     
| BRAKET                                    { Unconstrained }
| e1=dot_expr DOTDOT e2=dot_expr            { Interval(e1, e2) }
| e1=dot_expr RANGE e2=expr                 { Range(e1, e2) }
| e=dot_expr TICKRANGE i=pars(pnum)?        { TickRange(e, Common.option_map i get_num) }
                    
tickinit: TICK l=parlist(expr,COMMA)        { l }
		   
nexpr:
| e=expr                                    { ([], e) }
| l=label IMPLY e=expr                      { (l, e) }

label:
| e=expr                                    { [e] }
| e1=expr BAR l=label                       { e1 :: l }

adavalue:
| n=pnum                               { get_num n }
| c=CHAR                               { Adavalue.mk_char c }
| s=STRING                             { Adavalue.mk_string s }

pnum: n=NUM                            { mkloc $loc n }
                                       
%inline INFIX_OP:
| STAR    { times }
| STARSTAR { power }
| SLASH   { div }
| PLUS    { plus }
| MINUS   { minus }
| MOD     { modu }
| REM     { rem }
          
| AMPAND  { sconcat }
| LEQ     { leq }
| GEQ     { geq }
| LT      { lt }
| GT      { gt }
| EQUAL   { equal }
| NOTEQ   { notequal }

| AND THEN { band }
| AND     { band }
| OR      { bor }
| OR ELSE { bor }

%inline PREFIX_OP:
| NOT     { bnot }
| MINUS   { neg }       

generic: GENERIC { failwith "Cannot handle generic" }
    
garbage: l=garbage_token+ { l }
        
garbage_token:
ARRAY | ASSIGN | BEGIN | BODY | BRAKET | CASE | CHAR | COLON
  | COMMA | CONSTANT | DECLARE | DELAY | DOT | DOTDOT | ELSE
  | ELSIF | END | FOR | FUNCTION | IDENT | IF | IMPLY | IN
  | IS | LOOP | LPAREN | NULL | NUM | OF | OTHERS | OUT | PACKAGE
  | PROCEDURE | RANGE | RECORD | RENAMES | RETURN | RPAREN | SEMI
  | STRING | SUBTYPE | THEN | TICK | TICKRANGE | TYPE | USE | WHEN | WHILE
  | WITH         
  | ABS | AMPAND | AND | EQUAL | GEQ | GT | LEQ | LT | MINUS | MOD
  | NOT | NOTEQ | OR | PLUS | REM | SLASH
  |STAR | STARSTAR | XOR
  | ABSTRACT | ACCEPT | ACCESS | ALIASED
  | ENTRY | EXCEPTION | EXIT | GENERIC | GOTO
  | LIMITED | NEW | PRAGMA | PRIVATE | PROTECTED | RAISE | REQUEUE
  | SELECT | SEPARATE | TERMINATE | TASK
  | REVERSE | BAR | LTLT | GTGT
  | DO | TAGGED | UNTIL { () }

