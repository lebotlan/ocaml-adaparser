open Loc

type parser_error =
  | Mismatch of string * string
  | Ignored of string
  | Missing of string
  | Empty of string
  | Not_long_identifier

let err2s = function
  | Mismatch (s1, s2) -> "Expected " ^ s2 ^ " but found " ^ s1
  | Ignored s -> "Ignored: " ^ s
  | Not_long_identifier -> "This expression should be a long identifier."
  | Missing s -> "Missing " ^ s
  | Empty s -> "Empty " ^ s
                           
exception Syntax_error of parser_error loc

let syntax_error pos err = raise (Syntax_error { pos ; v = err } )

type lp_error = parser_error Loc.loc

let () = Printexc.register_printer
    (function
      | Syntax_error pel -> Some (Printf.sprintf "%s: %s" (loc2s pel) (err2s pel.v))
      | _ -> None)

type 'a pv =
  { pv: 'a ;
    errors: lp_error list }

let lp2s lpe = Printf.sprintf "%s: %s" (Loc.loc2s lpe) (err2s lpe.v)

let lperr2s ~margin l = Common.sep (fun e -> margin ^ lp2s e) "\n" l

let pv ?err x =
  let errors = match err with
    | None -> []
    | Some e -> [e]
  in
  { pv = x ; errors }

let map x f = { pv = f x.pv ; errors = x.errors }

let bind x f =
  let r = f x.pv in
  { pv = r.pv ;
    errors = List.rev_append x.errors r.errors }

let (>>=) = bind
  
let (>>::) x l = x >>= (fun vx -> l >>= (fun vl -> pv (vx :: vl)))

let pnil = { pv = [] ; errors = [] }

let p_map ll f =
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
