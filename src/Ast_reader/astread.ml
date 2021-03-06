open Astmap
open Astlib
open Ast
open Parse_errors
open Namespace
open Loc

(*** All errors ***)

type acupv = unit pv

let err_to_acu rv = { rv with acu = rv.rval >>= (fun _ -> rv.acu) }

let all_errors_map = object
  inherit [acupv] tree_mapper accumulates as super
  method! pl_declarations kind dlpv acu = err_to_acu (super#pl_declarations kind dlpv acu)      
  method! file fpv acu = err_to_acu (super#file fpv acu)
end

let all_errors fpv = (all_errors_map#file fpv punit).acu


(*** All procdecl ***)

type acudecl =
  { decls: (namespace * procdecl) list ;
    namesp: namespace }

let init_acu =
  { decls = [] ;
    namesp = init_namespace }

let insert_decl decl acu =
  { acu with decls = (acu.namesp, decl) :: acu.decls }

let block_exit inacu outacu =
  { decls = outacu.decls ;
    namesp = inacu.namesp }

let all_procdecl_map =
  object
    inherit [acudecl] tree_mapper { accumulates with block_exit }
        
    method! use_id li acu = return li { acu with namesp = insert_use li acu.namesp }
    method! pack_rename pr acu = return pr { acu with namesp = insert_pr pr acu.namesp }
    method! procdecl _kind decl acu = return decl (insert_decl decl acu)
  end

let all_procdecl fpv = (all_procdecl_map#content fpv.pv.content init_acu).acu.decls


(*** All with/use clauses ***)

type acu2 = withclause list
  
let all_wclauses_map =
  object
    inherit [acu2] tree_mapper accumulates    
    method! withclause wc acu = return wc (wc :: acu)
  end

let all_wclauses fpv = (all_wclauses_map#content fpv.pv.content []).acu


(*** All prodefs ***)

type acu3 = procdef list

let all_procdefs_map =
  object
    inherit [acu3] tree_mapper accumulates as super
    method! procdef def acu =
      (* Recursive call, otherwise we just get the top-level procedure. *)
      let- (_,acu) = super#procdef def acu in 
      def :: acu
  end

let all_procdefs fpv = (all_procdefs_map#content fpv.pv.content []).acu


(*** Declarations in a package spec. *)
let package_decls f =

  let rec loop = function
    (* The package content could not be found (package Foo is ...) *)
    | [] -> pv ~err:{ pos = f.fpos ; v = Missing "package content" } []

    (* We have found a package declaration *)
    | Package pc :: _ -> pc.package_declarations

    (* Ignore other top-level declarations *)
    | _ :: rest -> loop rest

  in

  loop f.content.pv
