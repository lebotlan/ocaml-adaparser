# ocaml-adaparser
Parser of an approximation of a subset of Ada 2005. Written in Ocaml + Menhir + Lwt + ocamllex

## Disclaimer
You won't be able to compile this project as is, because I use a couple of other libraries of mine which I have not included in the sources here (but which I can provide if anyone is interested).

## Interesting points
 - This project illustrates how to use menhir and ocamllex with lwt.
 - Additionally, it illustrates how to build a stateful lexer (each lexing function returns a new token AND a new lexer state).
 
 Files of interest: parser.mly, parse_errors.mli, parse_errors.ml, lexer.mli, lexer.mll, lexhelp.mli, lexhelp.ml
 
 
 
