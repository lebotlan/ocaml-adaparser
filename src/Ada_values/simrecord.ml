type 'a t =
  { cs: bool ;
    fields: (string * ('a ref)) list }

exception Badfield of string

let norm_cs cs nm = if cs then nm else String.lowercase_ascii nm

let create ~cs fields =
  { cs ;
    fields = List.map (fun (nm, v) -> (norm_cs cs nm, ref v)) fields }

let get_ref r field = 
  try List.assoc (norm_cs r.cs field) r.fields
  with Not_found -> raise (Badfield field)

let assign r field newv =
  let fieldref = get_ref r field in
  fieldref := newv

let read r field =
  let fieldref = get_ref r field in
  !fieldref

type 'a field =
  { fname: string ;
    ftype: 'a }
    
type 'a rtype =
  { tcs: bool ;
    tfields: 'a field list }

let belongs vbelongs rt r =
  let rec loop f1 f2 =
    match (f1, f2) with
    | [], [] -> true
    | _, [] | [], _ -> false
    | { fname ; ftype } :: o1, (name, vr) :: o2 -> fname = name && vbelongs ftype !vr && loop o1 o2
  in
  
  rt.tcs = r.cs &&
  let f1 = List.sort Stdlib.compare (List.map (fun { fname ; ftype } -> { fname = norm_cs r.cs fname ; ftype }) rt.tfields)
  and f2 = List.sort Stdlib.compare (List.map (fun (a,b) -> (norm_cs r.cs a, b)) r.fields) in

  loop f1 f2

  
let f2s v2s (name, vr) = Printf.sprintf "%s = %s ;" name (v2s !vr)

let r2s ?(margin="") v2s r = Printf.sprintf "%s(%s)" margin (Common.sep (f2s v2s) ("\n " ^ margin) r.fields)

let ft2s t2s ff = Printf.sprintf "%s: %s ;" ff.fname (t2s ff.ftype)

let rt2s t2s rt = Printf.sprintf "record\n    %s\nend record ;" (Common.sep (ft2s t2s) "\n    " rt.tfields)

let get_fields r = List.map (fun (n,_) -> norm_cs r.cs n) r.fields

let cmp f r1 r2 =

  let fields1 = get_fields r1
  and fields2 = get_fields r2 in

  let res = Stdlib.compare fields1 fields2 in

  if res <> 0 then res
  else
    try
      let rec loop = function
        | [] -> 0
        | x :: rest ->
          let v1 = read r1 x
          and v2 = read r2 x in

          let res = f v1 v2 in
          if res <> 0 then res
          else loop rest
      in          
      loop fields1

    with Badfield _ -> assert false (* They have the same fields !!! *)
      
  
