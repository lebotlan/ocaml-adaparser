open Astlib
open Ast
open Adaparser
open Readfile
open Astreader
open Astread
open Adanorm
open Parse_errors
(* open Namespacenorm *)
    
let test_proc =
  Adasig.{
    name = "Afficher" ;
    args = [ In "ArgA : Blabla.Integer" ; In "ArgB: bool" ] ;
    out = Some "Blabla.Float"
  }

let includedirs = [ "Ada" ; "/home/commetud/1ere Annee/ADA/Sources/GAda/" ]

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

      (* Print all errors *)
      let errs = all_errors p_file in
      Lwt_io.print "\n=== ERRORS ===\n\n" ;%lwt
      Lwt_list.iter_s (fun err -> Lwt_io.printf " * %s\n" (lp2s err)) errs.errors ;%lwt
      
      (* Normalise file: qualify, expand var init, flatten seq *)
      let%lwt (p_nfile, defs) = Nm_qualify.all_procdecl ~includedirs p_file in
      let p_nfile = (Astnorm.expand_var_init#file p_nfile ()).rval in
      let p_nfile = (Astnorm.norm_keep_semantics#file p_nfile []).rval in
      let p_nfile = (Astnorm.flatten_seq#file p_nfile ()).rval in
      
      Lwt_io.print "\n\n=== Normalized file ===\n\n" ;%lwt
      Lwt_io.printf "\n%s\n" (Astprint.pfile2s p_nfile) ;%lwt

      (* Print definitions *)
      Lwt_io.print "\n\n=== Definitions ===\n\n" ;%lwt
      Lwt_list.iter_s (fun (_,d) -> Lwt_io.printf " * %s\n\n" (Astprint.procdecl2s d.decl)) defs ;%lwt

      (* Compliance tests with hard-coded signature *)
      Lwt_io.printf "\n\nCompliance test (skipped).\n" ;%lwt
(*
      Lwt_list.iter_s
        begin fun (nmsp, decl) ->
          let result = Sigtest.complies (nmsp, decl) test_proc in
          Lwt_io.printf " * %s  ==>  %s\n" (l2s decl.procname) (Sigtest.result2s result)
        end
        decls ;%lwt
  *)    
      Lwt.return_unit
    in
    Lwt.return_unit

open Lwtlaunch.Setkeys

let () = noconfig ===> Lwtlaunch.launch ~appname:"Testada" ~run ()
