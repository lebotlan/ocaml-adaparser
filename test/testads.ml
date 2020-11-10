open Astlib
open Adanorm
open Parse_errors
open Idents

let includedirs = ["include" ; "Ada"]

let run () =
  if Array.length Sys.argv <> 2 then
      Lwt_io.printf "Usage: %s package-name (not path)\n" Sys.argv.(0)
  else
    let packname = Sys.argv.(1) in
    let lid = Text.split ~sep:"." packname in
    let lid = List.map (fun s -> Loc.mkdummy "testads" (Idents.norm s)) lid in

    let%lwt penv = Package_env.empty_pck_env includedirs ~packages:[lid] [] in
    let env = penv.pv in    
    
    let%lwt () =

      let pack = Package_env.pck_find env lid in
      
      Lwt_io.printf "\n===== Errors =====\n\n%s\n" (Common.sep lp2s "\n" penv.errors) ;%lwt
      Lwt_io.printf "\n===== ADS defs ====\n\n%s\n\n" (Common.sep (fun (i,_) -> l2s i) ", " pack.defs) ;%lwt
      
      Lwt.return_unit
    in
    Lwt.return_unit

open Lwtlaunch.Setkeys

let () = noconfig ===> Lwtlaunch.launch ~appname:"Testads" ~run ()
