
(* Nmspace: 
 *  - track opened packages (use), package renamings
 *  - characterize identifiers (arguments, variables, for-loop identifiers, ...) *)

open Astlib
open Ast
open Astreader
open Astmap
open Loc_env
open Package_env

class nmenv (env_init:decl_env) (pckenv_init:pck_env) =

  object(self:'s)
        
    (* Environment : variables, arguments, ...*)
    val decl_env = env_init

    (* Opened packages (use) *)
    val pckenv = pckenv_init

    (* All procdefs. *)
    val defs = ([] : ('s * procdef) list) ;

    method get_decl_env = decl_env
    method get_pck_env = pckenv
    method get_defs = defs
      
    method tos =
      Printf.sprintf "\n----- Namespace -----\n%s\n\n%s\n\n--------------------------\n\n"
        (env2s decl_env) (pck_env2s pckenv)

    method insert_env i tp = {< decl_env = insert_env decl_env i tp >}
    method insert_use li = {< pckenv = insert_use pckenv li >}    
    method insert_def def = {< defs = def :: defs >}

    method insert_with li =
      let first = match li with
        | [] -> assert false (* Empty package name ? *)
        | ii :: _ -> ii
      in
      
      (* Check the package is known. *)
      let _ = Package_env.pck_find pckenv li in
      
      (* Package A *)
      let pack = Package_env.pck_find pckenv [first] in
      
      self#insert_env first (Decl pack.found_pack.ads_decl)

    method init =
      Common.myfold (opened pckenv_init) self
        (fun me pack -> me#insert_with pack.found_pack.qualified)

    (* inacu#block_exit outacu *)
    method block_exit (outacu:'s) = {< defs = outacu#get_defs >}

    (* acu0#merge_pre *)
    method merge_pre = self

    (* acu0#merge_mid acu1 *)
    method merge_mid (acu1:'s) = {< defs = acu1#get_defs >}

    (* acu0#merge_end acu1 acu2 *)
    method merge_end (_:'s) (acu2:'s) = {< defs = acu2#get_defs >}
    
    method userfun : 's user_fun =
      { block_exit = (fun inacu outacu -> inacu#block_exit outacu) ;
        merge_pre = (fun ~acu0 -> acu0#merge_pre) ;
        merge_mid = (fun ~acu0 ~acu1 -> acu0#merge_mid acu1) ;
        merge_end = (fun ~acu0 ~acu1 ~acu2 -> acu0#merge_end acu1 acu2) }

  end

class ['a] envmap (userfun: 'a user_fun) =
  object(self)
    constraint 'a = #nmenv
      
    inherit ['a] tree_mapper userfun as super

    method! with_id li acu = return li (acu#insert_with li)
    
    method! use_id li acu =
      let- (li, acu) = self#long_id li acu in
      acu#insert_use li

    method! pack_rename pr acu =
      (* long_id is qualified when calling super. *)
      let- (pr, acu) = super#pack_rename pr acu in
      acu#insert_env pr.pack_alias (Decl (Rename pr))

    method! packnew ((id,_,_) as p) acu =
      let- (p, acu) = super#packnew p acu in
      acu#insert_env id (Decl (Packnew p))

    method! fun_rename fr acu =
      let- (fr, acu) = super#fun_rename fr acu in
      acu#insert_env fr.fun_alias.procname (Decl (Funrename fr))
    

    (*** Record local names. ***)

    method! for_id id acu = return id (acu#insert_env id Forid)
    method! when_id id acu = return id (acu#insert_env id Whenid)
    
    method! vardef kind vardef acu =
      let- (vd, acu) = super#vardef kind vardef acu in
      acu#insert_env vd.varname (Decl (Vardef vd))

    method! typedef typedef acu =
      let- (td, acu) = super#typedef typedef acu in
      acu#insert_env td.t_name (Decl (Typedef td))

    method! subtypedef subtypedef acu =
      let- (std, acu) = super#subtypedef subtypedef acu in
      acu#insert_env std.st_name (Decl (Subtype std))

    method! arg kind a acu =
      let- (a, acu) = super#arg kind a acu in
      match kind with
      | Typedef_arg | Proc_arg false -> acu (* Not binding *)
      | Proc_arg true -> acu#insert_env a.argname (Arg a) (* This is a binder *)

    method! procdef def acu =
      let- (pd, acu) = super#procdef def acu in
      (acu#insert_env pd.decl.procname (Decl (Procdef pd)))#insert_def (acu, pd)

  end
