open Term2
open Unix
open Astlib
open Adaparser
    
(* Set of files already seen. *)
let seen = Hashtbl.create 2000

let get_result astf =
  try%lwt
    let%lwt astf = astf in
    let result =
      match (Astread.all_errors astf).errors with
      | [] -> "0"
      | _ -> "X"
    in
    Lwt.return result
      
  with _ -> Lwt.return "E"

(* Process one existing, unseen, file *)
let process outch path name =
  Styled.(p fmt yellow "Processing %s.\n" name e) ;%lwt

  let command = Printf.sprintf "gnatgcc -c -gnats \"%s\"" path in
  let gcc_result = Lwt_unix.system command in

  let astfile = Readfile.lwt_parse_file path in
   
  let%lwt gcc = match%lwt gcc_result with
    | WEXITED 0 -> Lwt.return "0"
    | WEXITED _ -> Lwt.return "E"
    | WSIGNALED i -> Lwt.return ("SIGNALED " ^ string_of_int i)
    | WSTOPPED i -> Lwt.return ("STOPPED " ^ string_of_int i)
  in

  let%lwt ast_result = get_result astfile in

  Lwt_io.fprintf outch "%s,%s,%s,,,%s,\n" name ast_result gcc path

(* Analyses a file whose path is given in a line of the file list. *)
let do_line outch _linenb path =

  let name = Filename.basename path in
  
  if Sys.file_exists path then  
    let digest = Digest.file path in

    (* Is it a known file ? *)
    if Hashtbl.mem seen digest then
      Styled.(p fmt dgray " - File %s already seen.\n" name e)
    else
      begin
        let () = Hashtbl.add seen digest () in
        process outch path name
      end

  else
    (* File does not exist *)
    Styled.(p fmt red "File does not exist: %s\n" path e)

let run () =
  if Array.length Sys.argv <> 2 then
      Lwt_io.printf "Usage: %s file-with-list-of-ada-files\n" Sys.argv.(0)
  else
    let file = Sys.argv.(1) in
    let out_file = file ^ "-out.csv" in
    let%lwt () =
      Lwt_io.(with_file ~mode:output out_file
                (fun outch -> Lwtfile.iter_file ~file ~rm_empty_lines:true (do_line outch)))

    in
    Lwt.return_unit

open Lwtlaunch.Setkeys

let () = noconfig ===> Lwtlaunch.launch ~appname:"Readall" ~run ()
