open Astlib
open Ast
open Idents
open Astprint

type id_typ =
  | Decl of declaration
  | Forid
  | Whenid
  | Arg of arg

type decl_env = (loc_ident * id_typ) list
type env = decl_env

let empty_env = []

let same_name i (i2, _) = Idents.equal i i2

let trim = String.trim

(* let map_env env f = List.map (fun (i,t) -> (i, f t)) env *)

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

(*
let diff env1 env2 =
  let rec loop acu e2 =
    if env1 == e2 then acu
    else match e2 with
      | [] -> failwith "Ast_env.diff: not a sub-environment."
      | (i, _) :: xs -> loop (i :: acu) xs
  in
  loop [] env2
*)
    
