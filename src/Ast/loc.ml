(* Position in a file. *)
type pos = {
    start : Lexing.position ;
    endp  : Lexing.position ;
  }

let mkpos (p1, p2) = { start = p1 ; endp = p2 }

let dummy_lexpos name =
  Lexing.{ pos_fname = name ;
           pos_lnum = 0 ;
           pos_bol = 0 ;
           pos_cnum = 0 }

let dummypos name =
  let start = dummy_lexpos name in
  { start  ; endp = start }

let builtinpos = dummypos "-builtin-"

(* Localized value *)
type 'a loc = {
    pos : pos ;
    v   : 'a ;
  }

let mkloc pp v = { pos = mkpos pp ; v }

let mkdummy name v = { pos = dummypos name ; v }

let mkbuiltin v = { pos = builtinpos ; v }

let pos2s p = Printf.sprintf "File %s: line %d, characters %d-%d"
    p.start.pos_fname p.start.pos_lnum (p.start.pos_cnum - p.start.pos_bol)
    (p.endp.pos_cnum - p.start.pos_bol)


