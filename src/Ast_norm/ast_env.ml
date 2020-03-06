open Astlib
open Ast
open Idents
open Astprint

type id_typ =
  | Decl of declaration
  | Forid
  | Whenid
  | Arg of arg

type env = (loc_ident * id_typ) list

let empty_env = []

let same_name i (i2, _) = Idents.equal i i2

let trim = String.trim

let insert_env ?(may_replace=true) env i typ =
  if (not may_replace) && List.exists (same_name i) env then
    failwith ("Ast_env.insert_env: " ^ l2s i ^ " is already bound in the environment.")
  else (i,typ) :: env

let env_find env i =
  match List.find_opt (same_name i) env with
  | None -> None
  | Some (_,typ) -> Some typ

let id_typ2s = function
  | Decl d -> "{" ^ trim (declaration2b ~margin:"" d) ^ "}"
  | Forid -> "forid"
  | Whenid -> "whenid"
  | Arg arg -> "{ " ^ arg2s arg ^ "}"

let env2s env = Common.sep (fun (i, t) -> Printf.sprintf "%s (%s)" (l2s i) (id_typ2s t)) " ; " env


let mkbt name =
  let t_name = Loc.mkbuiltin (Idents.norm name) in
  (t_name, Decl (Typedef { t_name ; t_args = [] ; t_body = Abstract ; t_constrain = None }))

let null =
  let procname = Loc.mkbuiltin (Idents.norm "null") in
  (procname, Decl (Procdecl { procname ; args = [] ; rettype = None }))
               
(* String: should be an alias to an array type. 
 * Boolean: should be an enumeration type. *)
let builtin_env =
  [ mkbt "Integer" ; mkbt "Float" ; mkbt "Boolean" ; mkbt "String" ;
    null ;
  ]

