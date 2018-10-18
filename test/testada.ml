open Astlib
open Adaparser
open Readfile
    
let read_standard () =
  let p_file = parse_file Sys.argv.(1) in
  Printf.printf "\n%s\n" (Astprint.file2s p_file) ;
  Printf.printf "-- OK, parsed.\n%!" ;
  ()


let run () =
  if Array.length Sys.argv <> 2 then
      Lwt_io.printf "Usage: %s adafile\n" Sys.argv.(0)
  else
    let file = Sys.argv.(1) in
    let%lwt () =
      let%lwt p_file = lwt_parse_file file in
      Lwt_io.printf "\n%s\n" (Astprint.file2s p_file) ;%lwt
      Lwt_io.printf "-- OK, parsed.\n%!" ;%lwt
      Lwt.return_unit
    in
    Lwt.return_unit

open Lwtplus.Setkeys

let () = noconfig ===> Lwtplus.launch ~appname:"Testada" ~run ()
