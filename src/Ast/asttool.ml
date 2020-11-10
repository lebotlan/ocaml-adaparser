open Ast
open Loc
open Adavalues
open Parse_errors
    
let eq_id id e = match e.v with
  | Id x -> Idents.equal x id
  | _ -> false

let eq_longid li1 e =
  match e.v with
  | Qualified (li2, i2) -> Idents.long_equal li1 (li2 @ [i2])
  | _ -> false

let li2expr li = match List.rev li with
  | [] -> assert false
  | i1 :: iz -> { v = Qualified (List.rev iz, i1) ; pos = Idents.get_li_pos li }

    
let rec is_null e =
  eq_id Idents.l_null e
  ||
  eq_longid Idents.li_null e
  ||
  match e.v with
  | Seq (_,el) -> List.for_all is_null el
  | Value w -> Adavalue.(0 = cmp unit w)
  | _ -> false

let empty_file name =
  let path = "dummy-empty-file-" ^ name in
  
  { path ;
    content = pnil ;
    file_comments = [] ;
    fpos = Loc.dummypos path }


