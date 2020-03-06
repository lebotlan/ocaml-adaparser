open Astlib
open Adanorm
open Parse_errors
open Idents

let includedirs = ["Ada"]

let run () =
  if Array.length Sys.argv <> 2 then
      Lwt_io.printf "Usage: %s package-name (not path)\n" Sys.argv.(0)
  else
    let%lwt penv = Use_env.empty_use_env includedirs [] in
    let env = penv.pv in
    
    let packname = Sys.argv.(1) in

    let lid = Text.split ~sep:"." packname in
    let lid = List.map (fun s -> Loc.mkdummy "testads" (Idents.norm s)) lid in
    
    let%lwt () =

      let%lwt p_ads_defs = Use_env.lwt_read_ads env lid in

      Lwt_io.printf "\n===== Errors =====\n\n%s\n" (Common.sep lp2s "\n" p_ads_defs.errors) ;%lwt
      Lwt_io.printf "\n===== ADS defs ====\n\n%s\n\n" (Common.sep (fun (i,_) -> l2s i) ", " p_ads_defs.pv) ;%lwt
      
      Lwt.return_unit
    in
    Lwt.return_unit

open Lwtlaunch.Setkeys

let () = noconfig ===> Lwtlaunch.launch ~appname:"Testads" ~run ()
