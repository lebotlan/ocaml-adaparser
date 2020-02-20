(*** ADA  Abstract Syntax Tree ***)
open Idents
open Adavalues.Adavalue
open Parse_errors

(* Function arguments *)
type mode = In | Out | InOut

(* Package renaming *)
type pack_rename =
  { pack_alias: loc_ident ;
    pack_orig: long_ident }

type withclause =
  | With of long_ident
  | Use of long_ident
  | Usetype of long_ident

type procdecl =
  { procname: loc_ident ; 
    args: arg list ;
    
    (* Return type (for a function). *)
    rettype: long_ident option }
  
(* Procedure or function definition *)
and procdef =
  { decl: procdecl ;

    (* Local declarations (before the procedure or function BEGIN). *)
    declarations: pv_declaration list ;

    body: expr ;

    (* Comments found in body. *)
    proc_comments: string list }

and arg =
  { argname: loc_ident ;
    argtype: long_ident ;
    mode: mode ;
    argdefault: expr option }

and fun_rename =
  { fun_alias: procdecl ;
    fun_orig: long_ident }

(* Possibly labeled type *)
and ltype = loc_ident option * long_ident

and package_content =
  { package_name: long_ident ;
    package_sig: bool ;
    package_declarations: pv_declaration list ;
    package_comments: string list ;
    package_init: expr option }

and declaration =
  | Withclause of withclause
  
  (* Package renames *)
  | Rename of pack_rename

  (* Package instance (package is new) *)
  | Packnew of loc_ident * long_ident * (ltype list)

  | Package of package_content

  (* Type definition *)
  | Typedef of loc_ident * arg list * type_expr * (subt_constraint option)
  | Subtype of loc_ident * long_ident * (subt_constraint option)
  
  (* Definition of a function or procedure. *)
  | Procdef of procdef

  (* Only declaration *)
  | Procdecl of procdecl

  (* Function renames *)
  | Funrename of fun_rename

  (* Variable definition *)
  | Vardef of vardef

and pv_declaration = declaration list pv

(* Variable definition *)
and vardef =
  { varname: loc_ident ;
    const: bool ;
    vartype: type_expr ;
    constrain: subt_constraint option ;
    vinit: expr option }
  
(* access types are simply represented by "access".type *)

(* Type definition *)
and type_expr =
  (* The type is kept abstract for the moment *)
  | Abstract 

  (* Type *)
  | Typename of long_ident
  
  (* Enumeration *)
  | Enumerate of loc_ident list

  (* Record type *)
  | Record of vardef list

  (* Array type *)
  | Array of (expr list * long_ident)

  (* Delta, digits *)
  | Delta of expr adavalue * expr adavalue

(* Range *)
and subt_constraint =
  | Index_constraint of expr list (* e.g. T_Mat(1..12, 10..20) *)
  | Range_constraint of expr (* e.g. Integer range 0..50 *)

(*** EXPRESSIONS (including ranges) ***)

(* Expression *)
and expr =
  (* Immediate value (includes unit) *)
  | Value of expr adavalue

  (* Variable name *)
  | Id of loc_ident

  (* Record or array *)
  | Tuple of nexpr list

  (* Assignment
   * It is not possible to use a builtin function, since assignment concerns variables, arrays, and records. *)
  | Assign of expr * expr

  (* expr (eexpr1, eexpr2,...) 
   *   - application
   *   - array coordinates *)
  | App of expr * (nexpr list)

  (* e.name *)
  | Select of expr * loc_ident

  (* e1'e2 *)
  | Tick of expr * expr

  | If of expr * expr * expr
  | While of expr * expr
  | Exitwhen of expr

  (* For reverse? X of/in e1 loop e2 *)
  | For     of [`OF | `IN] * bool * loc_ident * expr * expr
               
  | Declare of declaration list * expr
  | Case    of expr * (when_clause list)
  | Return  of expr
  | Seq     of expr list
  | New     of long_ident * (expr list)
  | Is_in   of expr * expr

  (* Exception handler *)
  | Try     of expr * (when_clause list)

  (* Range expressions *)
  | Unconstrained (* <> *)
  | Interval of expr * expr (* 0..50 *)
  | TickRange of expr * (expr adavalue option) (* e.g. Bla'Range(2) *)
  | Range of expr * expr (* Integer range e *) 

and when_clause = 
  (* Match label: idents => block *)
  | Match  of loc_ident option * expr list * expr
  (* Others is just an identifier
   * Can match identifiers, ranges.
   * For exceptions, when x : exception  defines x. 
  *)

(* Note : Delay is a function application (builtin) *)

(* Named expression: label => expr *)
and nexpr = label * expr

(* List: (range1 | range2 => expr *)
and label = expr list

type file =
  { path: string ;
    content: pv_declaration list ;
    file_comments: string list ;

    (* Position of the whole file. *)
    fpos: Loc.pos }
 