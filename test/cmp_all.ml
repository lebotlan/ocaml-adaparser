open Astlib
open Ast
open Astreader
open Adaparser
open Readfile
open Adanorm
open Parse_errors

let includedirs = [ "include" ; "include/Act1" ; "/mnt/commetud/1ere Annee/ADA/Sources/GAda/" ]

(* Exception : procedure/function not found *)
exception Proc_not_found of string

let () = Printexc.register_printer
    (function
      | Proc_not_found s -> Some ("Proc not found : " ^ s)
      | _ -> None)


type found_proc =
  { infile: string ;
    proc: procdef }

(* Incomplete.... : declarations ??? *)
let same_procdef pd1 pd2 = Astcmp.cmp_expr pd1.body pd2.body = 0

(* Add this procdef into the acu. *)
let find_equal acu fp =

  let rec loop acu1 = function
    | [] -> (fp,1) :: acu1  (* New element *)
    | (fp1,k) as elt :: rest ->
      if same_procdef fp1.proc fp.proc then List.rev_append rest ((fp1,k+1) :: acu1)
      else loop (elt :: acu1) rest
  in

  loop [] acu
  

let find_proc procname p_nfile =

  let theone def = Idents.s_equal procname def.decl.procname.v in  
  (*   Printf.printf "Defs found in %s :\n      %s\n\n%!" p_nfile.pv.path (Common.sep (fun d -> Idents.l2s d.decl.procname) ", " (Astread.all_procdefs p_nfile)) ; *)
  
  match List.find_opt theone (Astread.all_procdefs p_nfile) with  
  | Some d -> d
  | None -> raise (Proc_not_found (Idents.i2s procname))
  

(* Get proc from file *)
let get_proc procname file =
  try%lwt
    (* Parse file *)
    let%lwt p_file = lwt_parse_file file in
    Lwt_io.printf "-- %s parsed.\n" file ;%lwt

    (* Normalise file: qualify, expand var init, flatten seq *)
    let%lwt (pu, p_nfile) = Nm_qualify.n_file ~includedirs ~var_prefix:"v" p_file in
    let p_nfile = (Astnorm.clear#file p_nfile ()).rval in
    let p_nfile = (Astnorm.expand_var_init#file p_nfile ()).rval in
    let p_nfile = (Astnorm.flatten_seq#file p_nfile ()).rval in
    let p_nfile = (Astnorm.norm_keep_semantics#file p_nfile []).rval in
    let p_nfile = (Astnorm.flatten_seq#file p_nfile ()).rval in

    let%lwt () =
      if pu.errors <> [] then
        begin
          Lwt_io.print "\n=== ADS ERRORS ===\n\n" ;%lwt
          Lwt_list.iter_s (fun err -> Lwt_io.printf " * %s\n" (lp2s err)) pu.errors ;%lwt
          Lwt.return_unit
        end
      else
        Lwt.return_unit
    in

    (* Look for procdef *)
    let proc = find_proc procname p_nfile in

    Lwt.return_some { infile = file ; proc }

  with _e ->
    (* Lwt_io.printf "Warning - parse error with file %s : %s\n" file (Printexc.to_string e) ;%lwt *)
    Lwt.return_none


let run () =
  if Array.length Sys.argv < 4 then
    Lwt_io.printf "Usage: %s procname adafile1 adafile2 ...\n" Sys.argv.(0)
  else
    let procname = Idents.norm (Sys.argv.(1)) in

    let files = match Array.to_list Sys.argv with
      | [] | [_] | [_;_] -> assert false
      | _cmd :: _procname :: rest -> rest
    in

    (* Get all matching procdefs *)
    let%lwt () = Lwt_io.printf "Parsing %d files...\n" (List.length files) in
    let%lwt procs = Lwt_list.filter_map_p (get_proc procname) files in
    let%lwt () = Lwt_io.printf "Parsed %d files (over %d)\n" (List.length procs) (List.length files) in

    (* Find equal procdefs *)
    let result = List.fold_left find_equal [] procs in
    let%lwt () =
      Lwt_io.printf "\n\n=============== Results ==================\n\n" ;%lwt
      let%lwt count = Lwt_list.fold_left_s
          begin fun count (_fp,k) ->
            if k = 1 then
              begin
(*
                Lwt_io.printf "\n\n---------------------- Single guy -----------------------\n\n%s\n\n"
                  (Astprint.expr2s ~margin:"     " fp.proc.body) ;%lwt
*)
                Lwt.return (count + 1)
              end
            else
              begin
                Lwt_io.printf "This guy has #%d copies\n" k ;%lwt
                Lwt.return count
              end
          end
          0 result
      in

      Lwt_io.printf "\n\n and %d guys are singles :\n\n" count

    in
    
    Lwt.return_unit

open Lwtlaunch.Setkeys

let () = noconfig ===> Lwtlaunch.launch ~appname:"Cmp_all" ~run ()
