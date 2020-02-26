open Astlib
open Astreader
open Adasig
open Idents
open Loc
open Ast    

(* For levenshtein distance *)
let max_errs = 2

(*
levenshtein distance in module Misc (see libref in the manual)
library is  compiler-libs.common
*)
  
type cmp_result =
  | Not_a_chance
  | Wrong of string
  | OK of string

let result2s = function
  | Not_a_chance -> "Not-a-chance"
  | Wrong s -> "Wrong : " ^ s
  | OK s -> "OK : " ^ s

let show_num = function
  | 0 -> "aucun"
  | 1 -> "un"
  | 2 -> "deux"
  | 3 -> "trois"
  | n -> string_of_int n

let wrong fmt = Printf.kprintf (fun s -> Wrong s) fmt

(*
type arg =
  | In of string
  | Inout of string

(* Procedure / function specification *)
type proc_descr =
  {
    (* Input arguments *)
    args: arg list ;
  *)

let get_arg_name = function
  | Adasig.In s | Inout s ->
    match Text.split ~sep:":" s with
    | [ name ; _ ] -> Text.strip name
    | _ -> assert false (* Bad argument spec. *)

let get_arg_type = function
  | Adasig.In s | Inout s ->
    match Text.split ~sep:":" s with
    | [ _ ; typ ] -> Text.strip typ
    | _ -> assert false (* Bad argument spec. *)

let get_arg_mode = function
  | Adasig.In _ -> Ast.In
  | Inout _ -> Ast.InOut

let rec last = function
  | [] -> failwith "list_last: empty list"
  | [x] -> x
  | _ :: xs -> last xs

(* String to long_ident *)
let s2li s =
  let l = Text.split ~sep:"." s in
  List.map (fun s -> Loc.mkdummy (norm s)) l

(*
 * Returns None if w1 is too different from w2
 * or Some distance otherwise. *)    
let levenshtein ~max_errs w1 w2 =
  let w1 = Text.lower w1
  and w2 = Text.lower w2 in

  (* Quick test *)
  if w1 = w2 then Some 0
  else Misc.edit_distance w1 w2 max_errs

(* Is type1 = type2 ? 
 * t1 : string
 * t2 : ast long_ident *)
let same_type title t1 (nmsp, t2) =

  let l1 = s2li t1 in
  if Namespace.ident_complies ~ref:l1 (nmsp,t2) then OK ""
  else
    let last1 = last l1
    and last2 = last t2 in

    match levenshtein ~max_errs (i2s last1.v) (i2s last2.v) with
    | None -> wrong "%s (ici %s) ne correspond pas à l'énoncé." title (li2s t2)
                
    (* If the last word is the same (Some 0), it means that the prefix is wrong. *)
    | Some 0 ->
      begin
        (* Look at the prefixes *)
        if List.length t2 = 1 then
          wrong "%s s'écrit bien %s, mais il manque quelque chose." title (li2s t2)
        else
          wrong "%s ne devrait pas être %s, car ce n'est pas la bonne version du type %s." title (li2s t2) (l2s last1)
      end

                         
    | Some _ -> wrong "%s s'écrit %s ou %s ?" title (l2s last1) (l2s last2)

(* Update best candidate if a better candidate has been found. *)
let update arg score (cand, queue) = match (score, cand) with
  | None, _ -> (cand, arg :: queue)
  | Some sc1, None -> (Some (arg, sc1), queue)
  | Some sc1, Some (arg2, sc2) ->
    if sc1 < sc2 then
      (* arg2 must not get lost: put it back in the queue. *)
      (Some (arg, sc1), arg2 :: queue)
    else
      (cand, arg :: queue)

(* Returns (arg, remaining-(untouched)-ast-args, distance-of-args-to-sig-arg) *)
let find_closest sig_arg ast_args =

  let sig_arg_name = get_arg_name sig_arg in
  
  let rec loop ((cand, untouched) as acu) = function
    | [] -> Common.option_map cand (fun (arg, dist) -> (arg, untouched, dist))
    | ast_arg :: other_ast_args ->
      loop (update ast_arg (levenshtein ~max_errs sig_arg_name (i2s ast_arg.argname.v)) acu) other_ast_args
  in
  loop (None, []) ast_args

(* Check if types and modes of both arguments coincide. *)
let check_arg nmsp sig_arg ast_arg =
  (* Check types. *)
  match same_type ("Le type de l'argument " ^ get_arg_name sig_arg) (get_arg_type sig_arg) (nmsp, ast_arg.argtype) with
  | Not_a_chance -> Not_a_chance
  | Wrong s -> Wrong s
  | OK s ->
    (* Check mode *)
    if get_arg_mode sig_arg = ast_arg.mode then OK s
    else wrong "Le mode (in ou in out) de l'argument %s n'est pas conforme à l'énoncé." (get_arg_name sig_arg) 
  
let check_args nmsp sig_args ast_args =

  let nargs = List.length sig_args in
  
  (* 1 - Check arguments according to their name. *)
  
  (* For each arg, find the closest matching name and check the pair. *)
  let rec loop ok_msg ast_args = function
    (* Finished. *)
    | [] -> assert (ast_args = []) ; OK ok_msg

    | sig_arg :: others ->
      
      begin match find_closest sig_arg ast_args with
        (* Arguments names are too different. We will check argument positions instead. *)
        | None -> Not_a_chance
          
        | Some (ast_arg, other_ast_args, dist) ->
          (* Check this pair. *)
          begin match check_arg nmsp sig_arg ast_arg with
            | Not_a_chance -> Not_a_chance
            | Wrong s -> Wrong s
            | OK s ->
              let msg2 =
                if ok_msg <> "" then ok_msg
                else
                if s = "" then
                  if dist > 0 then Printf.sprintf "Le nom de l'argument est %s ou %s ?" (get_arg_name sig_arg) (i2s ast_arg.argname.v)
                  else ""
                else s
              in
              loop msg2 other_ast_args others
          end
      end
      
  in
  match loop "" ast_args sig_args with
  | Not_a_chance ->
    (* 2 - Check arguments according to their position. *)

    let rec loop ast_args sig_args =
      match ast_args, sig_args with
      | [], [] -> ""
      | _, [] | [], _ -> assert false (* The number of arguments has already been checked. *)
      | ast_arg :: other_ast_args, sig_arg :: other_sig_args ->
        begin match check_arg nmsp sig_arg ast_arg with
          | Not_a_chance ->
            if nargs > 1 then "Les arguments ne sont pas du type attendu."
            else "L'argument n'est pas du type attendu."              
          | Wrong s -> s
          | OK "" -> loop other_ast_args other_sig_args
          | OK s -> s
        end
    in
    
    begin match loop ast_args sig_args with
      | "" ->
        if nargs > 1 then OK "La déclaration est correcte, mais les noms des arguments ne sont pas ceux indiqués dans l'énoncé."
        else OK "La déclaration est correcte, mais le nom de l'argument n'est pas celui indiqué dans l'énoncé."
    | s -> Wrong s
    end
    
  | (OK _ | Wrong _) as x -> x

let complies (nmsp, astp) sigp =

  let ast_name = Text.capitalize (i2s astp.procname.v)
  and sig_name = Text.capitalize sigp.name in
  
  (* Same function name ? *)
  match levenshtein ~max_errs ast_name sig_name with
  | None -> Not_a_chance
  | Some n ->
    if n > 0 then wrong "Est-ce %s ou %s ?" ast_name sig_name
    else
      (* OK, same function name. *)
      
      (* Procedure / function ? *)
      let test_fun = match sigp.out, astp.rettype  with
        | None, None -> OK ""
        | None, Some _
        | Some _, None -> wrong "La déclaration de %s ne correspond pas à l'énoncé." ast_name
        | Some t1, Some t2 -> same_type "Le type de retour" t1 (nmsp, t2)
      in
      
      (* Continue ? *)
      begin match test_fun with
        | Not_a_chance | Wrong _ -> test_fun
        | OK _ ->
          
          (* Number of arguments *)
          let ast_len = List.length astp.args
          and sig_len = List.length sigp.args in
          
          if ast_len <> sig_len then wrong "L'énoncé indique %s argument%s. J%s'en trouve %s."
              (show_num sig_len) (if sig_len > 1 then "s" else "") (if ast_len > 0 then "" else "e n" ) (show_num ast_len)
          else
            check_args nmsp sigp.args astp.args
      end
     
