type lexpos = Lexing.position

(* Position in a file. *)
type pos = { start: lexpos ;
             endp: lexpos }

val mkpos: lexpos * lexpos -> pos

val dummy_lexpos: string -> lexpos

val dummypos: string -> pos

val builtinpos: pos

(* Localized value *)
type 'a loc = { pos : pos ;
                v   : 'a }

val mkloc: lexpos * lexpos -> 'a -> 'a loc

val mkdummy: string -> 'a -> 'a loc

val mkbuiltin: 'a -> 'a loc

val pos2s: pos -> string


(* The list must not be empty *)
val range: 'a loc list -> pos
  

