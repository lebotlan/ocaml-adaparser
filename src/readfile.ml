open Lexhelp
open Astlib
open Ast
open Parse_errors

type path = string
  
(* Non-lwt version *)
let parse_file path =

  let chin = open_in path in
  let (lexbuf, feeder) = new_lexer path chin in
 
  try
    let past = Parser.file feeder lexbuf in
    close_in chin ;
    past >>= (fun ast -> pv { ast with path })
    
  with e ->
    close_in chin ;
    Printf.printf "Error : %s\n%!" (Printexc.to_string e) ;    
    Printf.printf "\nLexer state :\n   " ;
    let pos = lexbuf.lex_start_p in
    dumplex feeder lexbuf 10 ;
    failwith (Printf.sprintf "%s:%d:%d Sorry, this is an unexplained syntax error."
                pos.pos_fname pos.pos_lnum (1 + pos.pos_cnum - pos.pos_bol) )


module I = Parser.MenhirInterpreter

(* Lwt-version *)
let lwt_parse_file path =

  Lwt_io.(with_file ~mode:input path)
    begin fun chin ->
      let%lwt (lexbuf, feeder) = lwt_new_lexer path chin in

      let get_token () =
        let%lwt token = feeder lexbuf in
        Lwt.return (token, lexbuf.lex_start_p, lexbuf.lex_curr_p)
      in
      
      try%lwt
        let init = Parser.Incremental.file lexbuf.lex_curr_p in
        
        (* Parsing loop *)
        let rec loop last_valid_checkpoint cp = match cp with
          | I.Accepted v -> Lwt.return v
                            
          | I.InputNeeded _ ->
            let%lwt tokpos = get_token () in
            loop cp (I.offer cp tokpos)
              
          | I.Shifting _ | I.AboutToReduce _ | I.HandlingError _ -> loop last_valid_checkpoint (I.resume cp)
                                                                
          | I.Rejected -> Lwt.fail_with "Rejected."
        in

        let%lwt past = loop init init in          
        Lwt.return (past >>= (fun ast -> (pv { ast with path })))
                   
      with e ->
        Lwt_io.printf "Error : %s\n" (Printexc.to_string e) ;%lwt
        Lwt_io.printf "\nLexer state :\n   " ;%lwt
        let pos = lexbuf.lex_start_p in
        lwt_dumplex feeder lexbuf 10 ;%lwt
        Lwt.fail_with (Printf.sprintf "%s:%d:%d Sorry, this is an unexplained syntax error."
                         pos.pos_fname pos.pos_lnum (1 + pos.pos_cnum - pos.pos_bol) )
    end

