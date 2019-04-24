open Loc
    
module S = Shared_vals.Make (Shared_vals.HString)

type s = S.sh

let i2s x = Text.capitalize (S.tov x)

let l2s l = i2s l.v
let li2s li = Common.sep l2s "." li

(* Normalize and share *)
let norm s = S.share (Text.lower s)

let empty = S.share ""

let get_li_pos = function
  | [] -> assert false (* Empty long identifier ? *)
  | x :: _ -> x.pos

(* Identifiers *)
type loc_ident = s loc
type long_ident = loc_ident list

let empty_ident = Loc.mkdummy empty
let empty_long_ident = []

let equal s1 s2 = s1.v == s2.v
let s_equal s1 s2 = s1 == s2

let is_empty s = s.v == empty

let long_equal l1 l2 = List.for_all2 equal l1 l2
    
let access = norm "access"
let all = norm "all"
let others = norm "others"   
let i_exception = norm "exception"
let i_exit = norm "exit"
let i_true = norm "true"
    
