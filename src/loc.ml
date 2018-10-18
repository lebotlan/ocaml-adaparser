(* Position in a file. *)
type pos = {
    start : Lexing.position ;
    endp  : Lexing.position ;
  }

let mkpos (p1, p2) = { start = p1 ; endp = p2 }

let dummypos = { start = Lexing.dummy_pos ; endp = Lexing.dummy_pos }

(* Localized value *)
type 'a loc = {
    pos : pos ;
    v   : 'a ;
  }

let mkloc pp v = { pos = mkpos pp ; v }

let mkdummy v = { pos = dummypos ; v }

let loc2s l = Printf.sprintf "File %s: line %d, characters %d-%d"
    l.pos.start.pos_fname l.pos.start.pos_lnum (l.pos.start.pos_cnum - l.pos.start.pos_bol)
    (l.pos.endp.pos_cnum - l.pos.start.pos_bol)


