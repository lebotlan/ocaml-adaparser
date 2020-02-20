open Astlib
open Adaparser
open Readfile
open Astreader
open Astread
open Namespace
open Idents
open Adanorm
    
let test_proc =
  Adasig.{
    name = "Afficher" ;
    args = [ In "ArgA : Blabla.Integer" ; In "ArgB: bool" ] ;
    out = Some "Blabla.Float"
  }

let includedirs = [ "Ada" ]

let run () =
  if Array.length Sys.argv <> 2 then
      Lwt_io.printf "Usage: %s adafile\n" Sys.argv.(0)
  else
    let file = Sys.argv.(1) in
    let%lwt () =
      
      (* Parse file *)
      let%lwt p_file = lwt_parse_file file in
      Lwt_io.printf "\n%s\n" (Astprint.pfile2s p_file) ;%lwt
      Lwt_io.printf "-- OK, parsed.\n\n" ;%lwt

      (* Normalise file *)
      let%lwt p_nfile = Namespacenorm.n_file includedirs p_file.pv in
      Lwt_io.print "\n=== Normalized file ===\n\n" ;%lwt
      Lwt_io.printf "\n%s\n" (Astprint.pfile2s p_nfile) ;%lwt

      (* Find and print declarations *)
      let decls = all_procdecl p_file in      
      Lwt_list.iter_s (fun (n,d) -> Lwt_io.printf " * %s\n" (Astprint.procdecl2s (applynm_procdecl n d))) decls ;%lwt

      (* Compliance tests with hard-coded signature *)
      Lwt_io.printf "\n\nCompliance test.\n" ;%lwt

      Lwt_list.iter_s
        begin fun (nmsp, decl) ->
          let result = Sigtest.complies (nmsp, decl) test_proc in
          Lwt_io.printf " * %s  ==>  %s\n" (l2s decl.procname) (Sigtest.result2s result)
        end
        decls ;%lwt
      
      Lwt.return_unit
    in
    Lwt.return_unit

open Lwtlaunch.Setkeys

let () = noconfig ===> Lwtlaunch.launch ~appname:"Testada" ~run ()
