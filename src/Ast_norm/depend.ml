open Nm_qualify
open Astlib
open Idents
open Ast
    
type effects =
  { nms: qenv ;
    expr: expr ;
    read: loc_ident list ;
    writes: loc_ident list ;
    defines: loc_ident list ;
    side_effects: bool }

let get_effects nms e =
  { expr = e ;
    nms ;
    read = [] ;
    writes = [] ;
    defines = [] ;
    side_effects = false }

let rec disjoint l1 = function
  | [] -> true
  | i2 :: rest -> not (List.exists (Idents.equal i2) l1) && disjoint l1 rest

let commutable ea eb =
  (* A and B can commute whenever :
   *   1 - we don't have: side-effects(A) AND side-effects(B)  (otherwise, the order of side effects might be changed)
   *   2 - what A reads is not written by B
   *   3 - what B reads is not written by A
   *   4 - A and B do not write on the same variable
   *   5 - what A defines is disjoint from names read/written by B.
   *   6 - what B defines is disjoint from names read/written by A.
  *)

  not (ea.side_effects && eb.side_effects) (* 1 *)
  && disjoint ea.read eb.writes (* 2 *)
  && disjoint eb.read ea.writes (* 3 *)
  && disjoint ea.writes eb.writes (* 4 *)
  && disjoint ea.defines eb.read && disjoint ea.defines eb.writes (* 5 *)
  && disjoint eb.defines ea.read && disjoint eb.defines ea.writes (* 6 *)

let l_to_expr = function
  | [] -> assert false (* The acu is not supposed to contain empty lists. *)
  | [ eff ] -> eff.expr
  | l ->
    let ll = List.map (fun e -> e.expr) l in
    Loc.{ pos = range ll ; v = Seq (false, ll) }

let rec insert eff acu = function
  | [] -> insert_back eff [] acu (* Cannot go upwards *)
            
  | (subl :: rest) as ll ->
    let eff_commutes = List.for_all (commutable eff) subl in
    
    if eff_commutes then insert eff (subl :: acu) rest
    else insert_back eff ll acu

and insert_back eff ll = function
  | [] -> [eff] :: ll (* Empty acu *)
  | last :: rest -> List.rev_append rest ((eff :: last) :: ll)

let cut_seq nms el =
  let el = List.map (get_effects nms) el in
  
  (* res is a reversed list of lists of effects. *) 
  let rec loop res = function
    | [] -> Common.myrevmap res l_to_expr
    | p :: rest -> loop (insert p [] res) rest
  in

  loop [] el
  
