
module type TLEX =
sig
  type state
  type 'a t
      
  val init: state    
  val next_token: state -> Lexing.lexbuf -> (Parser.token * state) t
end

module type MONAD =
sig
  type 'a t
  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val fail : exn -> 'a t
  val on_refill : Lexing.lexbuf -> unit t
end

module Make(M:MONAD) : TLEX with type 'a t = 'a M.t

