open Adavalues.Adavalue
open Astlib.Ast
       
exception Bad_arguments of string

type v = expr adavalue

let bad_nargs l n title = raise (Bad_arguments (Printf.sprintf "(builtin) %s: bad number of arguments (%d instead of %d)" title (List.length l) n))

let rec apply_all env args = function
  | [] -> assert false
  | [ lastf ] -> lastf env args
  | onef :: otherf -> (try onef env args with _ -> apply_all env args otherf)

(* Overload a builtin function. We keep the name of the first function. *)
let overload = function
  | [] -> assert false (* Empty overloading ? *)
  | Builtin (name, f) :: others ->
    let all_fun = f :: List.map (function Builtin (_, f) -> f | _ -> assert false) others in   
    Builtin (name, fun env args -> apply_all env args all_fun)
  | _ -> assert false (* Not a builtin *)

let delay = Builtin ("delay",
                     (fun env -> function
                        | [ arg ] -> (Adaenv.delay env (get_float arg), unit)      
                        | l -> bad_nargs l 1 "delay"))

let araise = Builtin ("raise",
                      (fun _env -> function
                         | [ _arg ] -> assert false (* raise not implemented yet *)
                         | l -> bad_nargs l 1 "raise"))

let makeun get mk name op = Builtin (name, (fun env -> function
    | [ arg ] -> (env, mk (op (get arg)))
    | l -> bad_nargs l 1 name))

let makeop get mk name op = Builtin (name, (fun env -> function
    | [ arg1 ; arg2 ] -> (env, mk (op (get arg1) (get arg2)))
    | l -> bad_nargs l 2 name))

let iop n f = makeop get_int mk_int n f
let fop n f = makeop get_float mk_float n f

let cop n f = makeop get_int mk_bool n f
let fcop n f = makeop get_float mk_bool n f

let bop n f = makeop get_bool mk_bool n f

let ipower a b = int_of_float ((float_of_int a) ** (float_of_int b))

let times = overload [ iop "*" ( * ) ; fop "*" ( *. ) ]
let div   = overload [ iop "/" ( / ) ; fop "/" ( /. ) ]
let plus  = overload [ iop "+" (+) ; fop "+" (+.) ]
let minus = overload [ iop "-" (-) ; fop "-" (-.) ]
let power = overload [ iop "**" ipower ; fop "**" ( ** ) ]
let modu  = overload [ iop "mod" (mod) ; fop "mod" (mod_float) ]

let equal = overload [ cop "=" (=) ; fcop "=" (=) ; bop "=" (=) ]
let notequal = overload [ cop "/=" (<>) ; fcop "/=" (<>) ; bop "/=" (<>) ]
    
let geq   = overload [ cop ">=" (>=) ; fcop ">=" (>=) ]
let leq   = overload [ cop "<=" (<=) ; fcop "<=" (<=) ]
let gt    = overload [ cop ">" (>) ; fcop ">" (>) ]
let lt    = overload [ cop "<" (<) ; fcop "<" (<) ]

let bor   = bop "or" (||)
let band  = bop "and" (&&)
let bnot  = makeun get_bool mk_bool "not" (not)

let neg = overload [ makeun get_int mk_int "-" (fun i -> -i) ;
                     makeun get_float mk_float "-" (fun i -> -.i) ]

let sconcat = makeop get_string mk_string "&" (^)
