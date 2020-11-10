(*
open Nmspace
open Astlib
open Idents
open Ast
open Astreader.Astmap
open Astprint

(* TODO : reset side-effect ....; when ?. *)

let contains i l = List.exists (Idents.equal i) l

(* Populate the hashtable with the given information.
 * Format is [ package1, [ (proc1, false) ; (proc2, true) ; (etc.) ] 
               package2, ... ] *)
    
let pack_populate table all =
  Common.(myiter all begin fun (pack, l) ->
      (* packname => pack longid *)
      let packid = List.map (fun s -> mkbuiltin (norm s)) (String.split_on_char '.' pack) in

      (* Add all entries *)
      myiter l (fun (id, v) -> Hashtbl.add table (packid, mkbuiltin (norm id)) v)
    end)


class denv env use_env =
  object(o)
    inherit nmenv env use_env as super

    val reads = []
    val writes = []
    val defines = []
    val side_effects = false

    (* Cache for procedure in packages => has side effects. 
     * The key is (package id, proc id) *)
    val pack_side_effects = Hashtbl.create 200

    method populate_se_table l = pack_populate pack_side_effects l
    
    method get_reads = reads
    method get_writes = writes
    method get_side_effects = side_effects

    method write i = if contains i writes then o else {< writes = i :: writes >}
    method read i  = if contains i reads then o else {< reads = i :: reads >}
    method sidef   = if side_effects then o else {< side_effects = true >}

    method clone_from acu = {< reads = acu#get_reads ;
                               writes = acu#get_writes ;
                               side_effects = acu#get_side_effects >}

    (* Remove names that are scoped in outenv but not in inenv. *)
    method filters inenv outenv =
      let blacklist = Loc_env.diff inenv outenv in
      let filter_out l = List.filter (fun x -> not (List.exists (Idents.equal x) blacklist)) l in

      {< reads = filter_out reads ;
         writes = filter_out writes >}         

    method! block_exit outacu = ((super#block_exit outacu)#clone_from outacu)#filters o#get_env outacu#get_env
    method! merge_mid acu1 = (super#merge_mid acu1)#clone_from acu1        
    method! merge_end acu1 acu2 = (super#merge_end acu1 acu2)#clone_from acu2

    method extern_has_side_effects packid id adb =
      let key = (packid, id) in
      if Hashtbl.mem pack_side_effects key then Hashtbl.find pack_side_effects key
      else
        begin
          (* Populate the table with the package content. *)
          pack_populate pack_side_effects adb ;
          if Hashtbl.mem pack_side_effects key then Hashtbl.find pack_side_effects key
          else
            begin
              Printf.printf "*Warning* Depend.extern_has_side_effects: Procedure %s not found in package %s.\n" (l2s id) (li2s packid) ;
              true
            end
        end
        
    method! tos =      
      Printf.sprintf "%s--------------- DENV -----------------\n\n   reads : %s\n   writes : %s\n   defines : %s\n   side_effects : %b\n\n----------------------------------------- "
        super#tos
        (Common.sep l2s ", " reads) (Common.sep l2s ", " writes) (Common.sep l2s ", " defines) side_effects
    
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


(* Find a named argument among a function's argument. *)
let find_arg label f_args =
  try
    let farg = List.find (fun farg -> Idents.equal farg.argname label) f_args in
    (farg, List.filter (fun a -> a != farg) f_args)
    
  with Not_found ->
  begin match f_args with
    | [] -> assert false (* Already filtered by the caller. *)
    | x :: xs ->
      Printf.printf "*Warning* Function application: function does not have expected label %s\n%!" (l2s label) ;
      (x, xs)
  end

let find_select _ _ = assert false (* TODO FIXME *)

let effmap denv =
  object(o)
    inherit [denv] envmap denv#userfun as super

    method! expr_id ctxt li acu =
      match ctxt.as_pos with
      | LHS_leftmost -> return li (acu#write li)
      | RHS ->
        (* Is this a function or procedure (and with side-effects?) *)
        begin match Loc_env.env_find acu#get_env li with
          | Some (Decl (Procdef _pdef)) -> assert false (* TODO TODO FIXME *)
            
          | Some (Decl (Procdecl _)) ->
            (* We only have the declaration, not the body.
             * We assume it has side effects. *)
            Printf.printf "*Warning* Procdecl without body, we assume side-effects: %s\n%!" (l2s li) ;
            return li acu#sidef
            
          | None | Some (Forid | Whenid | Arg _ | Decl _) -> return li (acu#read li)
        end

    (* Invocation of a procedure in a package. *)
    method! select ctxt (e, id) acu =
      let res = super#select ctxt (e, id) acu in
      let (ee, iid) = res.rval in
      let ret a = return (ee, iid) a in

      (* Find if iid is a procedure/function in the package ee *)
      match find_select ee iid with
      | None -> ret acu
      | Some (_,_,None) ->
        (* We don't have the adb source.
         * We can only assume it has side effects. *)
        Printf.printf "*Warning* Extern proc without body, we assume side-effects: %s\n%!" (l2s iid) ;
        ret acu#sidef
        
      | Some (packid, Some adb) -> ret (if acu#extern_has_side_effects packid iid adb then acu#sidef else acu)

    method! app ctxt (e, nel) acu =
      let res = super#app ctxt (e, nel) acu in
      let (ee, l) = res.rval in

      (* Find if this is a function with [in]-out arguments. *)
      let acu2 =
        match ee.v with

        (* TODO FIXME, WE ONLY LOOK TO LOCAL PROC/FUN HERE, WE SHOULD CONSIDER SELECT(...) WHEN IT COMES FROM A PACKAGE. 
         * USE find_select then *)
        | Id fname ->
          (* Find the function in the environment. *)
          begin match Loc_env.env_find acu#get_env fname with
            | None ->
              Printf.printf "*Warning* Invocation of an unknown function %s\n%!" (l2s fname) ;
              res.acu

            | Some (Forid | Whenid) ->
              Printf.printf "*Warning* Invocation of a for/when identifier %s\n%!" (l2s fname) ;
              res.acu                
              
            | Some (Arg _) ->
              Printf.printf "*Warning* Invocation of an argument %s\n%!" (l2s fname) ;
              res.acu

            | Some (Decl (Procdef { decl ; _ } | Procdecl decl)) ->

              (* Examine the function declaration.
               * match arguments *)
              let rec loop acu f_args app_args =
                match f_args, app_args with
                | [], _ -> acu
                | _, [] ->
                  Printf.printf "*Warning* Too many arguments in the invocation of %s\n%!" (l2s fname) ;
                  acu

                | fx :: fxs, ([], ax) :: axs -> loop (o#match_arg acu fx ax) fxs axs
                  
                | _, ([{ Loc.v = Id label ; _ }], ax) :: axs ->
                  let (fx, fxs) = find_arg label f_args in
                  loop (o#match_arg acu fx ax) fxs axs

                | _, _ -> 
                  Printf.printf "*Warning* Curious invocation of function %s\n%!" (l2s fname) ;
                  acu
              in

              loop res.acu decl.args l

            | Some (Decl _) ->
              Printf.printf "*Warning* Invocation of an unexpected value %s\n%!" (l2s fname) ;
              res .acu
          end
            
        | _ -> res.acu
      in

      return (ee, l) acu2

    (* Check if an argument is in/out *)
    method match_arg acu farg arg =
      match farg.mode with
      | In -> acu
      | Out | InOut -> (o#expr { label_nm = S ; as_pos = LHS_leftmost } arg acu).acu
    
    (* DEBUG *)
    method! proc_body body acu =
      let res = super#proc_body body acu in
      Printf.printf "\n\n%s\n\n%s\n\n%!" (expr2s ~margin:"  " res.rval) res.acu#tos ;
      res
  end

let unseq_file (nmenv:#nmenv) file =

  let env = Loc_env.empty_env in
  let use_env = Package_env.reset_pck_env nmenv#get_pck_env in
  
  let init_denv = new denv env use_env in
  ((effmap init_denv)#file file init_denv).rval
  

    
*)
