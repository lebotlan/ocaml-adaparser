open Loc

type parser_error =
  | Mismatch of string * string
  | Ignored of string * int
  | Missing of string
  | Misplaced of string
  | Bad_symbol of string
  | Empty of string
  | Not_long_identifier
  | Cannot_find of Idents.long_ident
  | Unexpected_exn of exn

let err2s = function
  | Mismatch (s1, s2) -> "Expected " ^ s2 ^ " but found " ^ s1
  | Ignored (s,i) -> "Ignored: " ^ s ^ " length " ^ string_of_int i
  | Not_long_identifier -> "This expression should be a long identifier."
  | Missing s -> "Missing " ^ s
  | Empty s -> "Empty " ^ s
  | Misplaced s -> "Misplaced " ^ s
  | Bad_symbol s -> "Bad symbol " ^ s
  | Cannot_find li -> "Cannot find package " ^ (Idents.li2s li)
  | Unexpected_exn e -> "Unexpected_exn " ^ (Printexc.to_string e)
                           
exception Syntax_error of parser_error loc

let syntax_error pos err = raise (Syntax_error { pos ; v = err } )

type lp_error = parser_error Loc.loc

let lp2s lpe = Printf.sprintf "%s: %s" (Loc.pos2s lpe.pos) (err2s lpe.v)

let () = Printexc.register_printer
    (function
      | Syntax_error lpe -> Some (lp2s lpe)
      | _ -> None)

type 'a pv =
  { pv: 'a ;
    errors: lp_error list }

let pv ?cperr ?err x =
  let errors = match cperr,err with
    | None,None -> []
    | None, Some e -> [e]
    | Some pv, None -> pv.errors
    | Some pv, Some e -> e :: pv.errors
  in
  { pv = x ; errors }

let punit = pv ()
let pnil = pv []
    
let map x f = { pv = f x.pv ; errors = x.errors }

let bind x f =
  let r = f x.pv in
  { pv = r.pv ;
    errors = List.rev_append x.errors r.errors }

let (>>=) = bind
let (let>=) = bind
let (let>>) px f = let>= x = px in pv (f x)

let (and>=) a b =
  { pv = (a.pv, b.pv) ;
    errors = List.rev_append a.errors b.errors }

let (>>::) x l = x >>= (fun vx -> l >>= (fun vl -> pv (vx :: vl)))

let l_map ll f =
  let rec loop = function
    | [] -> pnil
    | x :: xs ->
      f x >>:: loop xs
  in
  ll >>= (fun l -> loop l)

open Loc

let swloc pvl =
  { pv = { pos = pvl.pos ;
           v = pvl.v.pv } ;
    errors = pvl.v.errors }

let swopt = function
  | None -> { pv = None ; errors = [] }
  | Some x -> { pv = Some x.pv ; errors = x.errors }
              
let swlist l = l_map { pv = l ; errors = [] } (fun x -> x)

let swpair1 pp =
  let (a,b) = pp.pv in
  ( { pv = a ; errors = pp.errors }, b )

let (>>>) x y = x >>= (fun () -> y)
(* let (let>>) x y = x >>> (y ()) *)

let unitjoin f l = List.fold_left (fun acu x -> acu >>= (fun () -> f x)) punit l

