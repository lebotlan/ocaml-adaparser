type dom =
  { lo: int ;
    hi: int }

type coords = int list

exception Bad_dimensions of int * int

exception Out_of_range of coords

type 'a t = {
  (* Visible dimensions of this simarray. *)
  visible_dims: dom list ;

  (* Function which maps coordinates to an index in the buffer. *)
  get_pos: (int list -> int) ;

  (* Buffer containing all data. *)
  buffer: 'a array ;
} 

let get_doms x = x.visible_dims

let domsize dom =
  assert (dom.hi - dom.lo + 1 >= 0) ;
  dom.hi - dom.lo + 1

let domlist dom =
  let rec loop x =
    if x > dom.hi then []
    else x :: loop (x+1)
  in
  loop dom.lo

let check_domain x dom =
  if x >= dom.lo && x <= dom.hi then ()
  else raise (Out_of_range [])

let check_pos doms pos =
  try List.iter2 check_domain pos doms
  with
  | Invalid_argument _ -> raise (Bad_dimensions (List.length doms, List.length pos))
  | Out_of_range _ -> raise (Out_of_range pos)

(* Default get_pos *)
let get_pos doms =

  (* acu = current position. 
   * size = size of the previous dimensions. *)
  let rec loop acu size d coords =
    match (d, coords) with
    | [], [] -> acu
      
    | [], r ->
      let ld = List.length doms in
      raise (Bad_dimensions (ld, ld + List.length r))
        
    | r, [] ->
      let ld = List.length doms in
      raise (Bad_dimensions (ld, ld - List.length r))

    | (d1 :: ds), (c1 :: cs) ->
      check_domain c1 d1 ;
      let size' = size * domsize d1
      and acu' = acu + (c1 - d1.lo) * size in

      loop acu' size' ds cs
  in
  begin fun coords ->
    try loop 0 1 doms coords
    with Out_of_range _ -> raise (Out_of_range coords)
  end

let create doms init =

  let total_size = List.fold_left (fun acu dom -> acu * domsize dom) 1 doms in

  { visible_dims = doms ;
    get_pos = get_pos doms ;
    buffer = Array.make total_size init }

let get sima coords = sima.buffer.(sima.get_pos coords)
let put sima coords v = sima.buffer.(sima.get_pos coords) <- v

let slice sima doms =
  let () =
    let len1 = List.length sima.visible_dims
    and len2 = List.length doms in
    
    if len1 = len2 then ()
    else raise (Bad_dimensions (len1, len2))
  in

  (* Check get_pos with low and high bounds, that is, check slice is included in sima. *)
  let _ = sima.get_pos (List.map (fun d -> d.lo) doms)
  and _ = sima.get_pos (List.map (fun d -> d.hi) doms) in
  
  { visible_dims = doms ;
    get_pos = (fun pos -> check_pos doms pos ; sima.get_pos pos) ; (* Magic: this is the same get_pos ! *)
    buffer = sima.buffer }

type 'a atype =
  { tdom: dom option list ;
    acontent: 'a }

let rec show_array margin v2s sima coords = function
  | [] -> v2s (get sima coords)
  | dom1 :: doms ->
    let indices = domlist dom1 in
    "(" ^
    (Common.sep (fun i -> Printf.sprintf "%3d => %s" i (show_array (margin ^ "        ") v2s sima (coords @ [i]) doms))
       (",\n" ^ margin) indices) ^
    ")"

let a2s ?(margin="") v2s ar = Printf.sprintf "%s" (show_array (margin ^ " ") v2s ar [] ar.visible_dims)

let dom2s dom = Printf.sprintf "%d..%d" dom.lo dom.hi

let coords2s c = "(" ^ (Common.sep string_of_int ", " c) ^ ")"

let odom2s = function
  | None -> "Integer range <>"
  | Some d -> dom2s d

let at2s t2s at = Printf.sprintf "array(%s) of %s" (Common.sep odom2s ", " at.tdom) (t2s at.acontent)

let rec dom_belongs dos dls = match (dos, dls) with
  | [], [] -> true
  | _, [] | [], _ -> false
  | None :: dos2, _ :: dls2 -> dom_belongs dos2 dls2
  | Some dom1 :: dos2, dom2 :: dls2 -> dom1 = dom2 && dom_belongs dos2 dls2

let belongs vbelongs at ar =
  dom_belongs at.tdom ar.visible_dims &&

  (* The whole buffer must contain valid values. No need to check only the current slice. *)
  Array.for_all (vbelongs at.acontent) ar.buffer
 


let _test () =

  let (lo1, hi1) = (10, 14)
  and (lo2, hi2) = (100, 102) in
  
  let ar2 = create [ { lo = lo1 ; hi = hi1 } ; { lo = lo2 ; hi = hi2 } ] "a" in
  for i = lo1 to hi1 do
    for j = lo2 to hi2 do
      Printf.printf "ar2(%d,%d) is at %d\n%!" i j (ar2.get_pos [i ; j]) ;
    done ;
  done ;

  Printf.printf "\n-----------------------------\n\n" ;
  
  let (slo1, shi1) = (12, 12)
  and (slo2, shi2) = (101, 102) in    

  let ar3 = slice ar2 [ { lo = slo1 ; hi = shi1 } ; { lo = slo2 ; hi = shi2 } ] in
  for i = slo1 to shi1 do
    for j = slo2 to shi2 do
      Printf.printf "ar3(%d,%d) is at %d\n%!" i j (ar3.get_pos [i ; j]) ;
    done ;
  done ;

  Printf.printf "\nArray2 : \n%s\n" (a2s ~margin:"         " (fun s -> s) ar2) ;
  Printf.printf "\nArray3 : \n%s\n" (a2s ~margin:"         " (fun s -> s) ar3) ;
  
  ()

(* let () = test () *)
    
