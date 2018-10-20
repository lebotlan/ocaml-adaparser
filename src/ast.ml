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

(* Phantom type used to distinguish procedure declarations from procedure definitions. *)
type def
type decl_only

type withclause = With of long_ident | Use of long_ident

(* Procedure or function definition 
 * 'a is def or decl_only. *)
type 'a procdef =
  { procname: loc_ident ; 
    args: arg list ;

    (* Return type (for a function). *)
    rettype: long_ident option ;

    (* Local declarations (before the procedure or function BEGIN).
     * EMPTY if this is a 'decl_only' *)
    declarations: (def declaration) list ;

    (* Body. = unit if 'decl_only'. *)
    body: expr ;

    (* Comments found in body. *)
    proccomments: string list ;

    (* Errors found in this procedure/function. *)
    sub_errors: unit pv ;
  }

and arg =
  { argname: loc_ident ;
    argtype: long_ident ;
    mode: mode ;
    argdefault: expr option }

and fun_rename =
  { fun_alias: decl_only procdef ;
    fun_orig: long_ident }

and ltype = loc_ident option * long_ident

and 'a declaration =
  (* Definition of a function or procedure. *)
  | Procdef of 'a procdef

  (* Package renames *)
  | Rename  of pack_rename

  (* Package instance (package is new) *)
  | Packnew of loc_ident * long_ident * (ltype list)

  (* Function renames *)
  | Funrename of fun_rename

  (* Type definition *)
  | Typedef of loc_ident * arg list * type_expr * (subt_constraint option)
  | Subtype of loc_ident * long_ident * (subt_constraint option)

  (* Variable definition *)
  | Vardef of vardef

  | Withclause of withclause

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

(*** EXPRESSIONS ***)

(* Expression *)
and expr =
  (* Immediate value (includes unit) *)
  | Value of expr adavalue

  (* Variable name *)
  | Id of long_ident

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

  (* e'name *)
  | Tick of expr * loc_ident

  | If of expr * expr * expr
  | While of expr * expr
  | Exitwhen of expr
  | For of loc_ident * expr * expr
  | Declare of (def declaration) list * expr
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
  | Range of expr * expr (* Integer range interval *)

and when_clause = 
  | Others of expr
  | Match  of expr list * expr

(* Note : Delay is a function application (builtin) *)


(* Named expression: label => expr *)
and nexpr = label * expr

(* List: (range1 | range2 => expr *)
and label = expr list

type 'a package =
  { package_name: long_ident ;
    package_declarations: ('a declaration) list ;
    package_comments: string list ;
    package_init: expr option }

type compilation_unit = 
  | Program of def procdef
  | Package_Sig  of decl_only package
  | Package_Body of def package
  | No_cu (* err: compilation unit was not found *)

type ast =
  { (* Clauses *)
    clauses: withclause list ;
    c_unit: compilation_unit }

type path = string

type file =
  { file: path ;
    ast: ast pv }

  
