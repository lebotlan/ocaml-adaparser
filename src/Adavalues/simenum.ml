exception Bad_value of string
exception Bad_range of string

(* All indexes (int) refer to the root parent. *)

type a_dense = float
type a_discrete = int
 
type 'a typ = {

  (* If None, this is a root type. *)
  root: 'a typ option ;

  (* Type name *)
  name: string ;

  (* Absolute index of the first value. *)
  first: 'a ;

  (* Absolute index of the last value. *)
  last: 'a ;

  (* Conversion functions *)
  tos: ('a -> string) ;
  ofs: (string -> 'a) ;
}

type dense_typ = a_dense typ
type discrete_typ = a_discrete typ

(* A value of type 'a t is basically a value of type 'a (int or float) *)
type 'a t =
  { typ: 'a typ ;
    pos: 'a }

type dense = a_dense t
type discrete = a_discrete t

type adaint = discrete

let norm_cs cs nm = if cs then nm else String.lowercase_ascii nm

let get_tname t = t.name

let rec get_root p =
  match p.root with
  | None -> p
  | Some p' -> get_root p'

let rec find_pos s pos = function
  | [] -> raise (Bad_value s)
  | x :: xs -> if x = s then pos else find_pos s (pos+1) xs

let check_range name typ n =
  if (typ.first <= n && n <= typ.last) then () (* OK *)
  else 
    raise (Bad_range (Printf.sprintf "%s: the given value (%s) is not in the expected range(%s-%s) " 
                        name (typ.tos n) (typ.tos typ.first) (typ.tos typ.last)))

let create_from_list ~cs ~name list =
  let list = List.map (norm_cs cs) list in
  { root = None ;
    name ;
    first = 0 ;
    last = List.length list - 1 ;
    tos = (fun v -> try List.nth list v with _ -> assert false) ;
    ofs = (fun s -> find_pos (norm_cs cs s) 0 list) }

let subcreate ~name ~parent ~first ~last =
  assert (get_root first.typ == get_root parent) ;
  assert (get_root last.typ == get_root parent) ;

  check_range "subcreate" parent first.pos ;
  check_range "subcreate" parent last.pos ;

  { root = Some (get_root parent) ;
    name ;
    first = first.pos ;
    last = last.pos ;
    tos = parent.tos ;
    ofs = parent.ofs }

let create_from_int_bij ~name ofs tos ~min ~max =
  { root = None ;
    name ;
    first = min ;
    last = max ;
    tos ;
    ofs }

let bool_enum = create_from_list ~cs:false ~name:"Boolean" ["true" ; "false"]

let unit_enum = create_from_list ~cs:false ~name:"Unit" ["null"]

let int_ofs s = try int_of_string s with Failure _ -> raise (Bad_value ("Not an integer : " ^ s))

let int_enum = create_from_int_bij ~name:"Integer" int_ofs string_of_int ~min:min_int ~max:max_int

let create_from_float ~name ~first ~last =
  { root = None ;
    name ;
    first ;
    last ;
    tos = string_of_float ;
    ofs = (fun s -> try float_of_string s with Failure _ -> raise (Bad_value s)) }

let float_enum = create_from_float ~name:"Float" ~first:min_float ~last:max_float

let char_enum = create_from_int_bij ~name:"Character"
    (fun s -> if String.length s = 1 then Char.code s.[0] else raise (Bad_value ("Not a char : " ^ s)))
    (fun v -> "'" ^ String.make 1 (Char.chr v) ^ "'")
    ~min:0 ~max:255

let belongs typ v = (get_root v.typ == get_root typ) && typ.first <= v.pos && v.pos <= typ.last

let is_int v = get_root v.typ == int_enum

let get_int v = if is_int v then v.pos else raise (Bad_value ("Expected an integer, got " ^ v.typ.name))

let is_float v = get_root v.typ == float_enum

let get_float v = if is_float v then v.pos else raise (Bad_value ("Expected a float, got " ^ v.typ.name))

let mkint n =
  { typ = int_enum ;
    pos = n }

let mkbool b =
  { typ = bool_enum ;
    pos = if b then 0 else 1 }

let is_bool v = get_root v.typ == bool_enum

let get_bool v = if is_bool v then 0 = v.pos else raise (Bad_value ("Expected a bool, got " ^ v.typ.name))

let mkchar c =
  { typ = char_enum ;
    pos = Char.code c }

let is_char v = get_root v.typ == char_enum

let get_char v = if is_char v then Char.chr v.pos else raise (Bad_value ("Expected a char, got " ^ v.typ.name))

let mkfloat f =
  { typ = float_enum ;
    pos = f }

let unit =
  { typ = unit_enum ;
    pos = 0 }
  
let e2s v = v.typ.tos v.pos

let ofs typ s = 
  { typ = typ ;
    pos = typ.ofs s }

let first typ = 
  { typ = typ ;
    pos = typ.first }

let last typ = 
  { typ = typ ;
    pos = typ.last }

let length typ = 
  let len = typ.last - typ.first + 1 in
  let len = if len >= 0 then len else max_int in
  mkint len

let range typ = (first typ, last typ)

let nth typ n =
  let nn = get_int n in
  check_range "nth" typ nn ;
  { typ = typ ;
    pos = n.pos }

let index v =
  check_range "index" v.typ v.pos ;
  mkint v.pos

let cmp v1 v2 = Pervasives.compare v1.pos v2.pos

let apply1 fname f v =
  let newpos = f v.pos in
  check_range fname v.typ newpos ;
  { typ = v.typ ;
    pos = newpos }

let succ = apply1 "succ" (fun x -> x+1)
let pred = apply1 "pred" (fun x -> x-1)

let apply_int f = apply1 "apply_int" f
let apply_float f = apply1 "apply_float" f

let apply2_int f v1 v2 = apply1 "apply2_int" (f v1.pos) v2
let apply2_float f v1 v2 = apply1 "apply2_float" (f v1.pos) v2

