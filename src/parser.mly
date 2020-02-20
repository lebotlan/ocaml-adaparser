
(*
 * This is a parser for a language that roughly looks like a subset of Ada.
 * This file must be compiled with menhir (a high-level parser generator for ocaml). 
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

let expected ?(may_be_empty=false) expected_string read_ident =
  if may_be_empty && is_empty read_ident then punit
  else
    if Idents.equal expected_string read_ident then punit
    else pv ~err:{ pos = read_ident.pos ; v = Mismatch (i2s expected_string.v, Idents.i2s read_ident.v) } ()
                        
let expected_long exp_longname read_longname =
  if Idents.long_equal exp_longname read_longname then punit
  else pv ~err:{ pos = get_li_pos read_longname ; v = Mismatch (li2s exp_longname, li2s read_longname) } ()

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
        if String.contains x '#' then failwith ("Cannot handle non decimal digits yet: " ^ x)
        else
          begin
            if String.contains x 'E' then Adavalue.mk_int (int_of_float (float_of_string x))
            else assert false (* Unknown number *)
            end
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
%token IF IN IS LIMITED  ISNEW
%token LOOP MOD NEW NOT 
%token NULL OF OR 
%token OUT PACKAGE PRIVATE 
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
                                                                                                                      
%start <Astlib.Ast.file Astlib.Parse_errors.pv> file

                                                 
(*** Precedence ***)

%left ACCESS
%left TICK
%left DOT

%left AMPAND

%left OR XOR
%left AND
      
%nonassoc LPAREN IN THEN RANGE ELSE DOTDOT
      
%nonassoc LEQ GEQ LT GT EQUAL NOTEQ

%nonassoc NOT

%left MOD REM         
%left PLUS MINUS ABS
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
%inline parlist(ELT, SEP): args = pars(separated_nonempty_list(SEP, ELT)) { args }

alt(X,Y): 
| x=X { x }
| y=Y { y }
                                    
(* Comma-list *)
comalist(ELT): elts = separated_nonempty_list(COMMA, ELT) { elts }

p_separated_nonempty_list(SEP, ELT):
| e=ELT { e >>:: pnil }
| e=ELT SEP l=p_separated_nonempty_list(SEP, ELT) { e >>:: l }
                        
p_comalist(ELT): elts=p_separated_nonempty_list(COMMA, ELT) { elts }

p_nonempty_list(ELT):
| e=ELT { e >>:: pnil }
| e=ELT l=p_nonempty_list(ELT) { e >>:: l }

empty: { }
            
p_list(ELT):
| empty { pv [] }
| l=p_nonempty_list(ELT) { l }

p_flatlist(ELT): pl=p_list(ELT) { let>> l = pl in List.flatten l }
      
(* Localized identifier *)
%inline loc_ident:
| id=IDENT { mkloc $loc id }
| ALL      { mkloc $loc all }
| ACCESS   { mkloc $loc access }

%inline simple_loc_ident:
| id=IDENT { mkloc $loc id }
     
(* Long name with dots only, e.g. Ada.Text_IO ou Package.varname *)
p_dotted_name: ids = p_separated_nonempty_list(DOT, p_v(simple_loc_ident)) { ids }

dotted_name: ids = separated_nonempty_list(DOT, simple_loc_ident) { ids }


(****************  FILES, COMPILATION UNITS  *****************)

file: decls=declaration* file_comments=EOF { pv { path = "" ;
                                                  (* Declarations are flattened *)
                                                  content = decls ;
                                                  file_comments ;
						  fpos = mkpos $loc } }

(* IDENTs found at the beginning of the file are ignored (and recorded as errors). *)                      
| _id=IDENT pf=file { let>= f = pf in pv ~err:(mkloc $loc(_id) (Ignored ("Ident", 1))) f }

(* Get rid of unused tokens warning. *)                    
| GTGT GTGT GTGT garbage_token EOF { assert false }
                 
(**************  DECLARATIONS AND DEFINITIONS  ***************)
declaration:

(* Private is ignored *)                                              
| PRIVATE            { pv [] }

(* Not implemented yet. *)
| GENERIC            { failwith "Cannot handle generic" }
| PROCEDURE loc_ident argsdef ISNEW { failwith "Procedure is new : not implemented" }
                                 
| c = withclause     { l_map c (fun x -> pv (Withclause x)) }

(* Package renames *)
| PACKAGE i=loc_ident RENAMES p=dotted_name SEMI  { pv [ Rename { pack_alias = i ;
								  pack_orig = p }]}
                                            
(*  Package instance (is new) *)                                           
| PACKAGE i=loc_ident ISNEW o=dotted_name l=parlist(ltype, COMMA) SEMI
                                                  { pv [ Packnew (i, o, l) ] }
| pc=package_sig        { map pc (fun pc -> [ Package pc ]) }
| pc=package_body       { map pc (fun pc -> [ Package pc ]) }

(* Type defs *)                        
| TYPE l=loc_ident SEMI                                         { pv [ Typedef (l, [], Abstract, None) ] }
| TYPE l=loc_ident args=argsdef alt(IS,ISNEW)
              pt=type_expr withprivate? r=subt_constraint? SEMI  { let>> t = pt in [ Typedef (l, args, t, r) ] }

| TYPE l=loc_ident args=argsdef IS RANGE e=expr SEMI            { pv [Typedef (l, args, Typename empty_long_ident, Some (Range_constraint e)) ] }
                                                        
| SUBTYPE l=loc_ident IS p=dotted_name r=subt_constraint? SEMI  { pv [ Subtype (l, p, r) ]}
				                                                
(* Function renaming *)
| p=procdecl RENAMES i=dotted_name SEMI { let>> decl = p in [ Funrename { fun_alias = decl ;
                                                                          fun_orig = i }] }
                                       
(* Constant or variable, but type is missing *)
| v=loc_ident i=initialize? SEMI   { pv ~err:(mkloc $loc (Missing "type"))
				       [ Vardef { varname = v ;
						  const = false ;
						  vartype = Typename empty_long_ident ;
                                                  constrain = None ;
						  vinit = i }]}
                                    
| vdef=vardef { l_map vdef (fun d -> pv (Vardef d)) }
              
| z=procdecl SEMI { let>> d = z in [Procdecl d] }
| z=procdef       { let>> d = z in [Procdef d] }

(* 'with' or 'use' clause *)
withclause: 
| WITH names=p_comalist(p_dotted_name) SEMI { l_map names (fun n -> pv (With n)) }
| USE  names=p_comalist(p_dotted_name) SEMI { l_map names (fun n -> pv (Use n)) }
| USE TYPE names=p_comalist(p_dotted_name) SEMI { l_map names (fun n -> pv (Usetype n)) } 

(* Ignored *)
withprivate: WITH PRIVATE { }
                   
p_declarations: d=p_flatlist(declaration) { d }
                   
                   
(********************  PACKAGES  *********************)
        
package_sig: 
PACKAGE package_name=dotted_name IS 
        pdecl=declaration*
        package_comments=END endname=dotted_name SEMI
                                                                                     
			 { expected_long package_name endname
                           >>>
			     pv { package_name ;
                                  package_sig = true ;
                                  package_declarations = pdecl ;
                                  package_comments ;
                                  package_init = None }
                         }
                                          
package_body:
PACKAGE BODY package_name=dotted_name IS
        pdecl=declaration*
        ob=init_block?
        package_comments=END endname=dotted_name SEMI
                                          
                         { expected_long package_name endname
                           >>>                  
                             let>> b = swopt ob in
			     { package_name ;
                               package_sig = false ;
                               package_declarations = pdecl ;
                               package_comments ;
                               package_init = b }
			 }

init_block: BEGIN b=exn_block     { b }
                   
                   

(****************  PROCEDURES AND FUNCTIONS  *****************)

procdecl:
| PROCEDURE procname=loc_ident args=argsdef
                                      { pv { procname ;
                                             args ;
                                             rettype = None } }
| FUNCTION procname=loc_ident args=argsdef RETURN ret=typename
                                      { pv { procname ;
                                             args ;
                                             rettype = Some ret } }
                                          
(* Arguments *)
argsdef: l=loption(parlist(arg, SEMI)) { List.flatten l }
                                                        
arg: names=comalist(loc_ident) COLON mode=mode? argtype=typename argdefault=argdefault?
                                  { Common.mymap names (fun argname -> { argname ;
									 argtype ;
									 mode = Common.option_default mode In ;
									 argdefault })
				  }

argdefault: ASSIGN e=expr { e }

mode: 
| IN { In }
| OUT { Out }
| IN OUT { InOut }

                                      
procdef:
|      pd = procdecl IS
           declarations=declaration*
       BEGIN
           body=exn_block?    
           coms=END endname=loc_ident? SEMI
							
                         { let>= body =
		             match body with
		             | None -> pv ~err:(mkloc $loc(body) (Missing "body")) vun
		             | Some b -> b

			   and>= decl = pd in
			   
			   expected ~may_be_empty:true decl.procname (Common.option_default endname empty_ident)
			   >>>                           
			     pv { decl ;
				  declarations ;
				  body ;
				  proc_comments = coms } }
                                  
(* Empty procedure *)                  
|  pd = procdecl IS?
   coms=END endname=loc_ident SEMI
                                  
                   { let>= decl = pd in
                     expected decl.procname endname
                     >>>
                       let kw = match decl.rettype with None -> "procedure " | _ -> "function " in
                       pv ~err:(mkloc $loc (Empty (kw ^ i2s decl.procname.v)))
			  { decl ;
                            declarations = [] ;
                            body = vun ;
                            proc_comments = coms } }

%inline vardef:
(* Constant or variable declaration *)
| names=comalist(loc_ident) COLON ALIASED? cst=CONSTANT?
   vt=vartype? i=initialize? SEMI
		   {
                     let const = cst=Some () in
                     
                     match vt with
                     | None ->
                        pv (Common.mymap names
					 (fun v -> { varname = v ;
						     const ;
						     vartype = Typename empty_long_ident ;
						     constrain = None ;
						     vinit = i }))
                        
                     | Some (pt,c) ->
                        let>> t = pt in
                        (Common.mymap names
				      (fun v -> { varname = v ;
						  const ;
						  vartype = t ;
						  constrain = c ;
						  vinit = i })) }

vartype: t=type_expr c=subt_constraint?    { (t, c) }
                         

typename:
| t=dotted_name                    { t }
(*| ALIASED t=dotted_name                    { t } *)
| ACCESS ALL? t=typename           { (mkloc $loc access) :: t  }
| t=typename TICK i=loc_ident      { t @ [i] }
| EXCEPTION                        { [mkloc $loc i_exception] }
                      
ltype:
| l=loc_ident IMPLY t=typename         { (Some l, t) }
| t=typename                           { (None, t) }                        

             
(* Spec : subtype_constraint is a range_constraint, digits_constraint, delta_constraint, index_constraint, or discriminant_constraint. *)
(* We ignore digits_constraint, delta_constraint, discriminant_constraint *)
subt_constraint:
| l=parlist(expr, COMMA)                                 { Index_constraint l }
| RANGE r=expr                                           { Range_constraint r }
            
initialize: ASSIGN e=expr { e }
      
      
(************************  TYPEDEFS  *************************)
type_expr:
| TAGGED? LIMITED? PRIVATE                             { pv (Abstract) }
| t=typename                                           { pv (Typename t) }
| l=parlist(loc_ident, COMMA)                          { pv (Enumerate l) }
| RECORD a=vardef* END RECORD                          { let>> l = swlist a in Record (List.flatten l) }
| ARRAY ranges=parlist(expr,COMMA) OF t=typename       { pv (Array (ranges, t)) }
| DELTA x=pnum DIGITS y=pnum                           { pv (Delta (get_num x, get_num y)) }

(***********************  STATEMENTS  ************************)

block: pl=p_nonempty_list(statement)                   { let>> l = pl in Seq l }

exn_block: pb=block e=exn_handler?                      { match e with None -> pb
                                                                     | Some pl ->
									let>> l = pl and>= b = pb in
									Try (b,l) }

exn_handler:
| EXCEPTION l=p_nonempty_list(when_clause)   { l }

in_or_of:
| IN                          { `IN }
| OF                          { `OF }

statement: 
| NULL SEMI                                                        { pv vun }
| DELAY e=expr SEMI                                                { pv (App (Value Builtins.delay, [([], e)])) }
| RAISE e=expr SEMI                                                { pv (App (Value Builtins.araise, [([], e)])) }
| RAISE SEMI                                                       { pv (App (Value Builtins.araise, [])) }
| GOTO e=expr SEMI                                                 { pv (App (Value Builtins.goto, [([], e)])) }
| EXIT SEMI                                                        { pv (Id (mkloc $loc i_exit)) }
              
| e=expr SEMI                                                      { pv e }                 

| e1=expr ASSIGN e2=expr SEMI                                      { pv (Assign (e1, e2)) }

| IF e1=expr THEN s1=block s2=elsif END IF SEMI                    { let>> s1 = s1 and>= s2 = s2 in If (e1, s1, s2) }

| FOR l=loc_ident iof=in_or_of rv=REVERSE? r=expr
  LOOP ps=block END LOOP SEMI                                      { let>> s = ps in For (iof, rv=Some (), l, r, s) }
						     
| WHILE e=expr LOOP ps=block END LOOP SEMI                         { let>> s = ps in While (e, s) }

| LOOP ps=block END LOOP SEMI                                      { let>> s = ps in While (Id (mkloc $loc i_exit), s) }

| bl=block_label? DECLARE
       pd=p_declarations
  BEGIN ps=exn_block END el=loc_ident? SEMI
  								   {
								     expected ~may_be_empty:true
									      (Common.option_default bl empty_ident)
									      (Common.option_default el empty_ident)
                                                                     >>>
								       let>> s = ps
								       and>= d = pd in
								       Declare (d, s) }

| EXIT WHEN e=expr SEMI                                            { pv (Exitwhen e) }

| CASE e=expr IS pl=p_nonempty_list(when_clause) END CASE SEMI     { let>> l = pl in Case (e, l) }

| RETURN e=expr SEMI                                               { pv (Return e) }
| RETURN SEMI                                                      { pv (Return vun) }

| BEGIN ps=exn_block END SEMI                                      { let>> s = ps in Declare ([], s) }
             
elsif:
| ELSIF e1=expr THEN ps1=block ps2=elsif                           { let>> s1 = ps1 and>= s2 = ps2 in If (e1, s1, s2) }
| ELSE s2=block                                                    { s2 }
| empty                                                            { pv vun }


block_label: i=loc_ident COLON                                     { i }

(* Arrrh, a curse on LR1 grammars! *)								   
when_clause: 
| WHEN i=loc_ident COLON l=separated_nonempty_list(BAR, expr) ib=imply_block  { ib (Some i) l }
| WHEN e=expr BAR l=separated_nonempty_list(BAR, expr) ib=imply_block         { ib None (e :: l) }
| WHEN e=expr ib=imply_block                                                  { ib None [ e ] }
(* Others is an identifier *)
		   
imply_block:  IMPLY ps=block                                      { fun lbl l -> let>> s = ps in Match (lbl, l, s) }
									  


(***********************  EXPRESSIONS  ************************)

(* Expressions which may be followed by a DOT or PARENTHESIS *)
dot_expr:
| v=adavalue                                { Value v }					    
| l=loc_ident                               { Id l }
| e=dot_expr DOT l=loc_ident                { Select (e, l) }
| e=dot_expr a=parlist(nexpr, COMMA)        { App (e, a) }
| l=parlist(nexpr, COMMA)                   { Tuple l }
| e1=dot_expr TICK e2=dot_expr              { Tick (e1,e2) }
                        
expr:
| e=dot_expr                                { e }
| NEW l=dotted_name                         { New (l, []) }
| NEW n=dotted_name TICK e=dot_expr         { New (n, [e]) }
| NEW n=dotted_name a=parlist(expr, COMMA)  { New (n, a) }
| e1=expr op=INFIX_OP e2=expr               { App (Value op, [ ([], e1) ; ([], e2) ]) }
| op=PREFIX_OP e=expr                       { App (Value op, [ ([], e) ]) }
| e=expr IN r=expr                          { Is_in (e, r) }
| e=expr NOT IN r=expr                      { App (Value bnot, [ ([], Is_in (e, r)) ]) }

(* Range expressions *)                                     
| BRAKET                                    { Unconstrained }
| e1=dot_expr DOTDOT e2=expr                { Interval(e1, e2) }
| e1=dot_expr RANGE e2=expr                 { Range(e1, e2) }
| e=dot_expr TICKRANGE i=pars(pnum)?        { TickRange(e, Common.option_map i get_num) }
                    
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
| XOR     { bxor }

%inline PREFIX_OP:
| NOT     { bnot }
| MINUS   { neg }
| PLUS    { idplus }
| ABS     { abs }


garbage_token:
| ABS | ABSTRACT | ACCEPT | ALL
| ACCESS | ALIASED | AND 
| ARRAY | BEGIN | BODY 
| CASE 
| CONSTANT | DECLARE | DELAY | DELTA | DIGITS
| DO | ELSE | ELSIF 
| ENTRY | EXCEPTION | EXIT 
| FOR | FUNCTION | GENERIC | GOTO 
| IF | IN | IS | LIMITED | ISNEW
| LOOP | MOD | NEW | NOT 
| NULL | OF | OR 
| OUT | PACKAGE | PRIVATE 
| PROCEDURE | PROTECTED | RAISE | RANGE 
| RECORD | REM | RENAMES | REQUEUE 
| RETURN | REVERSE | SELECT | SEPARATE 
| SUBTYPE | TAGGED | TASK | TERMINATE 
| THEN | TYPE | UNTIL | USE 
| WHEN | WHILE | WITH | XOR
| DOT | LT | LPAREN | PLUS | 
| BAR | AMPAND | STAR | RPAREN
| SEMI | MINUS | SLASH | COMMA
| GT | COLON | EQUAL | TICK | TICKRANGE
| DOTDOT | LTLT | BRAKET | LEQ
| STARSTAR | NOTEQ | GTGT | GEQ
| ASSIGN | IMPLY
             { }
             
