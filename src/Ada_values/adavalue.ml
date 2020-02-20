type id = string

type 'a adavalue =
  | Uninitialized
  | Enum of Simenum.discrete
  | Enumd of Simenum.dense
  | Array of 'a adavalue Simarray.t
  | Record of 'a adavalue Simrecord.t
  | Fun of 'a funvalue
  | Builtin of (string * (Adaenv.env -> ('a adavalue) list -> (Adaenv.env * 'a adavalue)))

and 'a funvalue =
  { fname: id ;
    args: 'a arg list ;
    content: 'a }

and 'a arg =
  { arg_name: id ;
    arg_default: 'a adavalue option }

and 'a adatype = {
  typename: string ;
  default: 'a adavalue ;
  tcontent: 'a tcontent }

and 'a tcontent =
  | TRecord of 'a adatype Simrecord.rtype
  | TArray of 'a adatype Simarray.atype
  | TEnum of Simenum.discrete_typ
  | TEnumd of Simenum.dense_typ

(* We cannot make a generic function :( because of value restriction.
 * The result must be polymorphic *)
let t_int =
  { typename = "Integer" ;
    default = Uninitialized ;
    tcontent = TEnum Simenum.int_enum }

let t_bool =
  { typename = "Boolean" ;
    default = Uninitialized ;
    tcontent = TEnum Simenum.bool_enum }

let t_char =
  { typename = "Character" ;
    default = Uninitialized ;
    tcontent = TEnum Simenum.char_enum }

let t_unit =
  { typename = "Unit" ;
    default = Uninitialized ;
    tcontent = TEnum Simenum.unit_enum }

let t_float =
  { typename = "Float" ;
    default = Uninitialized ;
    tcontent = TEnumd Simenum.float_enum }

let t_string =
  { typename = "String" ;
    default = Uninitialized ;
    tcontent = TArray (Simarray.{ tdom = [ None ] ;
                                  acontent = t_char }) }

let mk_enum f v = Enum (f v)
let mk_enumd f v = Enumd (f v)

let mk_int n = mk_enum Simenum.mkint n
let mk_char c = mk_enum Simenum.mkchar c
let mk_bool b = mk_enum Simenum.mkbool b
let mk_float f = mk_enumd Simenum.mkfloat f

let unit = Enum (Simenum.unit)

let mk_string s =
  let open Simarray in
  let len = String.length s in

  (* Printf.printf "Creating string '%s'\n%!" s ; *)
  
  let ar = create [ {lo = 1 ; hi = len} ] Uninitialized in

  (* Init string array *)
  let rec loop i =
    if i > len then ()
    else
      (put ar [i] (mk_char s.[i-1]) ;
       loop (i+1))
  in
  loop 1 ;
  
  Array ar

let rec belongs tt vv = match tt.tcontent, vv with
  | _, Uninitialized -> true

  | TRecord rt, Record rv -> Simrecord.belongs belongs rt rv
  | TRecord _, _ -> false
    
  | TArray at, Array ar -> Simarray.belongs belongs at ar
  | TArray _, _ -> false

  | TEnum te, Enum ev -> Simenum.belongs te ev
  | TEnumd td, Enumd ed -> Simenum.belongs td ed
  | (TEnum _ | TEnumd _), _ -> false

let rec tos ?(margin="") v = match v with
  | Uninitialized -> "*uninitialized*"
  | Enum e -> Simenum.e2s e
  | Enumd e -> Simenum.e2s e
  | Array ar ->
    if belongs t_string v then "\"" ^ get_string v ^ "\""
    else Simarray.a2s ~margin tos ar
                  
  | Record r -> Simrecord.r2s ~margin tos r
  | Fun fv -> fun2s fv
  | Builtin (b, _) -> b

and fun2s fv =
  Printf.sprintf "(FUN %s(%s))" fv.fname (Common.sep arg2s ", " fv.args)

and arg2s arg = arg.arg_name

and get_string v = match v with
  | Array ar ->
    if not (belongs t_string v) then failwith "Not a string." ;
    begin match Simarray.get_doms ar with
      | [ dom ] ->
        let len = dom.hi - dom.lo + 1 in
        String.init len (fun i -> get_char (Simarray.get ar [dom.lo + i]))
        
      | _ -> assert false (* A string is a one-dimension array. *)
    end           
  | x -> raise (Simenum.Bad_value ("Expected a string, got ada value " ^ tos x))

and get_char = function
  | Enum d -> Simenum.get_char d 
  | x -> raise (Simenum.Bad_value ("Expected a char, got ada value " ^ tos x))


let rec ttos adat = match adat.tcontent with
  | TRecord rt -> Simrecord.rt2s ttos rt
  | TArray  at -> Simarray.at2s ttos at
  | TEnum dt -> Simenum.get_tname dt
  | TEnumd dt -> Simenum.get_tname dt

let get_int = function
  | Enum d -> Simenum.get_int d
  | x -> raise (Simenum.Bad_value ("Expected an integer, got ada value " ^ tos x))

let get_float = function
  | Enumd d -> Simenum.get_float d
  | x -> raise (Simenum.Bad_value ("Expected a float, got ada value " ^ tos x))

let get_bool = function
  | Enum d -> Simenum.get_bool d
  | x -> raise (Simenum.Bad_value ("Expected a bool, got ada value " ^ tos x))

