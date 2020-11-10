open Loc
    
module S = Shared_vals.Make (Shared_vals.HString)

type s = S.sh

let i2s x = Text.capitalize (S.tov x)

let l2s l = i2s l.v
let li2s li = Common.sep l2s "." li

(* Normalize and share *)
let norm s = S.share (Text.lower s)

let prefix i s = norm (i ^ S.tov s)

let empty = S.share ""

let get_li_pos li = Loc.range li

(* Identifiers *)
type loc_ident = s loc
type long_ident = loc_ident list

let li_cmp li1 li2 = Stdlib.compare (List.map (fun i -> i.v) li1) (List.map (fun i -> i.v) li2) 

let empty_ident = Loc.mkdummy "Idents.empty" empty
let empty_long_ident = []

let equal s1 s2 = s1.v == s2.v
let s_equal s1 s2 = s1 == s2

let is_empty s = s.v == empty

let long_equal l1 l2 = try List.for_all2 equal l1 l2 with _ -> false

let i_exception = norm "exception"
let i_access = norm "access"
let i_all = norm "all"
let i_exit = norm "exit"
let i_true = norm "true"
let i_false = norm "false"
let i_null = norm "null"
let i_stdlib = norm "Stdlib"

let mkd i = mkdummy "Idents" i

let l_true = mkd i_true
let l_false = mkd i_false
let l_null = mkd i_null
let l_stdlib = mkd i_stdlib

let li_stdlib = [ l_stdlib ]

let std l = [ l_stdlib ; l ]

let li_null = std l_null
let li_true = std l_true
let li_false = std l_false

let pack2filename li =
  let open Text in
  
  let res = Text.lower (Common.sep l2s "-" li) in  
  let len = length res in

  (* Special rule for a- g- i- s-, see spec. *)
  match sub res 0 2 with
  | "a-" | "g-" | "i-" | "s-" -> get res 0 ^ "~" ^ sub res 2 (len - 2)
  | _ -> res

let reloc pos li = List.map (fun i -> { i with pos } ) li


let rec i_assoc_opt i = function
  | [] -> None
  | (i2,v) :: rest -> if equal i i2 then Some v else i_assoc_opt i rest

let rec li_assoc_opt li = function
  | [] -> None
  | (li2,v) :: rest -> if long_equal li li2 then Some v else li_assoc_opt li rest


let hashli li = Hashtbl.hash (List.map (fun i -> i.v) li)

module Hli =
struct
  type t = long_ident
  let equal = long_equal
  let hash = hashli
end

module Hashli = Hashtbl.Make(Hli)
    
