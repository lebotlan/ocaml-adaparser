
(* Nmspace: 
 *  - track opened packages (use), package renamings
 *  - characterize identifiers (arguments, variables, for-loop identifiers, ...) *)

open Astlib
open Ast
open Astreader
open Astmap
open Ast_env
open Use_env


class nmenv (env_init:env) (use_init:use_env) =
  object(self:'s)
        
    (* Environment : variables, arguments, ...*)
    val env = env_init

    (* Opened packages (use) *)
    val use = use_init

    (* All procdefs. *)
    val defs = ([] : ('s * procdef) list) ;
    
    method get_env = env
    method get_use_env = use
    method get_defs = defs
      
    method tos =
      Printf.sprintf "\n----- Namespace -----\n%s\n\n%s\n\n--------------------------\n\n"
        (env2s env) (use_env2s use)

    method insert_env i tp = {< env = insert_env env i tp >}
    method insert_use li   = {< use = insert_use use li >}
    
    method insert_def def  = {< defs = def :: defs >}

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
      
    (* With: the first package name is now bound in the environment. *)
    method! with_id li acu =
      (* I don't think li should be expanded first. 
       * If needed, invoke self#expand_longid (instead of acu#expand_longig) *)
      (*      let- (li, acu) = acu#expand_longid ~warn:false li in *)

      let- (li, acu) = return li acu in
      
      let first = match li with
        | [] -> assert false (* Empty package name ? *)
        | ii :: _ -> ii
      in      
      acu#insert_env first (Decl (Use_env.empty_package first))
    
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
