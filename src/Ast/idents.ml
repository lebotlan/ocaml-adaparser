open Loc
    
module S = Shared_vals.Make (Shared_vals.HString)

type s = S.sh

let i2s x = Text.capitalize (S.tov x)

let l2s l = i2s l.v
let li2s li = Common.sep l2s "." li

(* Normalize and share *)
let norm s = S.share (Text.lower s)

let empty = S.share ""

let get_li_pos li = match li with
  | [] -> assert false (* Empty long identifier ? *)
  | i1 :: _ ->
    (* Get last one *)
    let rec loop = function
      | [] -> assert false
      | [ik] -> { start = i1.pos.start ; endp = ik.pos.endp }
      | _ :: rest -> loop rest
    in
    loop li

(* Identifiers *)
type loc_ident = s loc
type long_ident = loc_ident list

let empty_ident = Loc.mkdummy "Idents.empty" empty
let empty_long_ident = []

let equal s1 s2 = s1.v == s2.v
let s_equal s1 s2 = s1 == s2

let is_empty s = s.v == empty

let long_equal l1 l2 = try List.for_all2 equal l1 l2 with _ -> false
    
let access = norm "access"
let all = norm "all"
let others = norm "others"   
let i_exception = norm "exception"
let i_exit = norm "exit"
let i_true = norm "true"
    

let pack2file li =
  let open Text in
  
  let res = Text.lower (Common.sep l2s "-" li) ^ ".ads" in  
  let len = length res in

  (* Special rule for a- g- i- s-, see spec. *)
  match sub res 0 2 with
  | "a-" | "g-" | "i-" | "s-" -> get res 0 ^ "~" ^ sub res 2 (len - 2)
  | _ -> res
