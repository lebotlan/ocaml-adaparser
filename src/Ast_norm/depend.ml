open Nmspace
open Astlib
open Idents
open Ast
open Astreader.Astmap

(* Remove names that are scoped in outenv but not in inenv. *)
  
class denv env use_env =
  object(o)
    inherit nmenv env use_env as super

    val reads = []
    val writes = []
    val defines = []
    val side_effects = false

    method reads = reads
    method writes = writes
    method side_effects = side_effects      

    method clone_from acu = {< reads = acu#reads ;
                               writes = acu#writes ;
                               side_effects = acu#side_effects >}

    method filters inenv outenv =
      let blacklist = Ast_env.diff inenv outenv in
      let filter_out l = List.filter (fun x -> not (List.exists (Idents.equal x) blacklist)) l in

      {< reads = filter_out reads ;
         writes = filter_out writes >}         

    method! block_exit outacu = ((super#block_exit outacu)#clone_from outacu)#filters o#get_env outacu#get_env
    method! merge_mid acu1 = (super#merge_mid acu1)#clone_from acu1        
    method! merge_end acu1 acu2 = (super#merge_end acu1 acu2)#clone_from acu2

    method write i = {< writes = i :: writes >}
    
  end

type effects =
  { denv: denv ;
    expr: expr ;
    reads: loc_ident list ;
    writes: loc_ident list ;
    defines: loc_ident list ;
    side_effects: bool }


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
  && disjoint ea.reads eb.writes (* 2 *)
  && disjoint eb.reads ea.writes (* 3 *)
  && disjoint ea.writes eb.writes (* 4 *)
  && disjoint ea.defines eb.reads && disjoint ea.defines eb.writes (* 5 *)
  && disjoint eb.defines ea.reads && disjoint eb.defines ea.writes (* 6 *)

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

let get_effects _ = assert false

let cut_seq nms el =
  let el = List.map (get_effects nms) el in
  
  (* res is a reversed list of lists of effects. *) 
  let rec loop res = function
    | [] -> Common.myrevmap res l_to_expr
    | p :: rest -> loop (insert p [] res) rest
  in

  loop [] el

let expr_first_var _ = assert false


let effmap denv =
  object
    inherit [denv] envmap denv#userfun as super

    method! core_expr pos ln e acu =
      let re = super#core_expr pos ln e acu in
      match re.rval with
      | Assign (e1, _) ->
        (* :=  something is written here *)
        begin match expr_first_var e1 with
          | None -> re
          | Some i -> return e (acu#write i)
        end
        
      | _ -> re
  end


let unseq_proc (nmenv, procdef) =  
  let init_denv = new denv nmenv#get_env nmenv#get_use_env in
  ((effmap init_denv)#procdef procdef init_denv).rval
    
