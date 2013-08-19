open Format

module A1 = struct
  type t = (int * int) option ref
  type manager = unit

  let copy x = ref (!x)
  let bottom man = ref None
  let top man = ref (Some(min_int,max_int))
  let canonical man (x:t) = match !x with
    | Some(a,b) when a>b ->  x := None
    | _ -> ()
  let is_bottom man (x:t) =
    canonical man x;
    !x=None
  let is_top man (x:t) =
    canonical man x;
    match !x with
    | None -> false
    | Some(a,b) when a>min_int || b<max_int -> false
    | _ -> true
  let is_leq man (x:t) y =
    canonical man x; canonical man y;
    match (!x,!y) with
    | (None,_) -> true
    | (Some _, None) -> false
    | (Some(a1,b1),Some(a2,b2)) ->
      let r = a1>=a2 && b1<=b2 in
      (* Printf.printf "leq : [%i,%i] <= [%i,%i] : %b\n%!" a1 b1 a2 b2 r; *)
      r

  let join man (x:t) y =
    canonical man x; canonical man y;
    match (!x,!y) with
    | (None,_) -> copy y
    | (_,None) -> copy x
    | (Some(a1,b1),Some(a2,b2)) ->
	ref (Some((min a1 a2),(max b1 b2)))
  let join_list man tx =
    List.fold_left (fun res x ->
      join man res x) (bottom man) tx
  let widening man (x:t) y =
    canonical man x; canonical man y;
    match (!x,!y) with
    | (None,_) -> copy y
    | (_,None) -> failwith ""
    | (Some(a1,b1),Some(a2,b2)) ->
	ref (Some(
	  (if a2<a1 then min_int else a2),
	  (if b2>b1 then max_int else b2)))
  let print man fmt (x:t) =
    canonical man x;
    match !x with
    | Some(a,b) ->
	if is_top man x then pp_print_string fmt "top"
	else
	  fprintf fmt "[%i,%i]" a b
    | None -> pp_print_string fmt "bot"
  let meet man (x:t) y =
    canonical man x; canonical man y;
    match (!x,!y) with
    | (None,_) -> ref None
    | (_,None) -> ref None
    | (Some(a1,b1),Some(a2,b2)) ->
	let a = max a1 a2 in
	let b = min b1 b2 in
	ref (if a>b then None else Some(a,b))
  let setcst man (x:t) (cst:int) =
    canonical man x;
    match !x with
    | None -> x
    | Some(a,b) -> ref (Some(cst,cst))
  let addcst man (x:t) (cst:int) =
    canonical man x;
    match !x with
    | None -> x
    | Some(a,b) -> ref (Some(
	(if a=min_int then a else a+cst),
	(if b=max_int then b else b+cst)))
  let leqcst man (x:t) (cst:int) =
    canonical man x;
    match !x with
    | None -> x
    | Some(a,b) ->
	let i = ref(Some(min_int,cst)) in
	meet man (x:t) i
  let geqcst man (x:t) (cst:int) =
    canonical man x;
    match !x with
    | None -> x
    | Some(a,b) ->
	let i = ref(Some(cst,max_int)) in
	meet man x i
end

module A2 = struct
  type t = A1.t * A1.t
  type manager = unit

  let bottom man =
    let bot = A1.bottom man in (bot,bot)
  let top man =
    let top = A1.top man in (top,top)
  let is_bottom man (x,y) =
    A1.is_bottom man x || A1.is_bottom man y
  let canonical man ((x,y) as abs) =
    A1.canonical man x; A1.canonical man y;
    if is_bottom man abs then begin
      x := None; y := None
    end
  let is_leq man (x1,y1) (x2,y2) =
    A1.is_leq man x1 x2 && A1.is_leq man y1 y2
  let join man (x1,y1) (x2,y2) =
    (A1.join man x1 x2, A1.join man y1 y2)
  let join_list man tx =
    List.fold_left (fun res x ->
      join man res x) (bottom man) tx
  let widening man (x1,y1) (x2,y2) =
    (A1.widening man x1 x2, A1.widening man y1 y2)
  let print man fmt ((x,y) as v) =
    canonical man v;
    if is_bottom man v then pp_print_string fmt "bot"
    else fprintf fmt "(%a,%a)" (A1.print man) x (A1.print man) y
  let meet man (x1,y1) (x2,y2) =
    (A1.meet man x1 x2, A1.meet man y1 y2)
  let setcst man ((x,y) as v) (dim:int) (cst:int) =
    if is_bottom man v
    then v
    else if dim=0 then (A1.setcst man x cst, A1.copy y)
    else if dim=1 then (A1.copy x, A1.setcst man y cst)
    else failwith ""

  let assignvar man ((x,y) as v) (dim1:int) (dim2:int) =
    if is_bottom man v || dim1==dim2
    then v
    else if dim1=0 then (y,y)
    else if dim1=1 then (x,x)
    else failwith ""

  let addcst man ((x,y) as v) (dim:int) (cst:int) =
    if is_bottom man v || cst==0
    then v
    else if dim=0 then (A1.addcst man x cst, A1.copy y)
    else if dim=1 then (A1.copy x, A1.addcst man y cst)
    else failwith ""
  let leqcst man ((x,y) as v) (dim:int) (cst:int) =
    if is_bottom man v
    then v
    else if dim=0 then (A1.leqcst man x cst, A1.copy y)
    else if dim=1 then (A1.copy x, A1.leqcst man y cst)
    else failwith ""
  let geqcst man ((x,y) as v) (dim:int) (cst:int) =
    if is_bottom man v
    then v
    else if dim=0 then (A1.geqcst man x cst, A1.copy y)
    else if dim=1 then (A1.copy x, A1.geqcst man y cst)
    else failwith ""

end

module Vertex = struct
  type t = string
  let compare (i:string) j = Pervasives.compare i j
  let hash (i:string) = Hashtbl.hash i
  let equal (i:string) j = i = j

  let print ppf s = Format.pp_print_string ppf s
end

module Hedge = struct
  type t = int
  let compare (i:int) j = Pervasives.compare i j
  let hash i = Hashtbl.hash i
  let equal (i:int) j = i = j

  let print ppf i = Format.pp_print_int ppf i

  let counter = ref (-1)
  let new_hedge () = incr counter; !counter
end

module T = struct

  type vertex = Vertex.t
  type hedge = Hedge.t
  module Vertex = Vertex
  module Hedge = Hedge

  (* module VertexSet = Set.Make(Vertex) *)
  (* module HedgeSet = Set.Make(Hedge) *)
  (* module VertexTbl = Hashtbl.Make(Vertex) *)
  (* module HedgeTbl = Hashtbl.Make(Hedge) *)

  let print_vertex = Vertex.print
  let print_hedge = Hedge.print

end

module Manager = struct
  module H = Hgraph.Make(T)
  include A2
  type abstract = A2.t

  type vertex_attribute = unit
  type hedge_attribute = unit
  type graph_attribute = unit

  (* let apply hedge tabs = *)
  (*   let abs = tabs.(0) in *)
  (*   let nabs = *)
  (*     match hedge with *)
  (*     | 01 -> A2.setcst () abs 0 0 *)
  (*     | 12 -> A2.setcst () abs 1 0 *)
  (*     | 23 -> A2.leqcst () abs 0 99 *)
  (*     | 210 -> A2.geqcst () abs 0 100 *)
  (*     | 34 -> A2.addcst () abs 0 1 *)
  (*     | 45 -> A2.leqcst () abs 0 49 *)
  (*     | 59 -> A2.addcst () abs 1 1 *)
  (*     | 47 -> A2.geqcst () abs 0 50 *)
  (*     | 79 -> A2.addcst () abs 1 (-1) *)
  (*     | 92 -> abs *)
  (*     | _ -> failwith "" *)
  (*   in *)
  (*   nabs *)

  let apply hedge () tabs =
    let abs = tabs.(0) in
    let nabs =
      match hedge with
      | 01 -> A2.setcst () abs 0 0
      | 12 -> A2.setcst () abs 1 0
      | 23 -> A2.leqcst () abs 0 99
      | 26 -> A2.geqcst () abs 0 100
      | 34 -> A2.addcst () abs 0 1
      | 45 -> A2.assignvar () abs 1 0
      | 52 -> abs
      | _ -> failwith ""
    in
    [|nabs|], []

(* Creation of the following equation graph:
   X0: x=0;
   X1: y=0;
   X2: while (x<=99) do
   X3:   incr x;
   X4:   y = x;
   X5: done
*)


  let abstract_init i =
    if i = "v0"
    then A2.top ()
    else A2.bottom ()

  type function_id
  module Function_id = struct
    type t = function_id
    let compare _ _ = assert false
    let equal _ _ = assert false
    let hash _ = assert false
  end

  let find_function _ = assert false
  let clone_vertex _ = assert false
  let clone_hedge _ = assert false

end

module FP = Hgraph.Fixpoint(Manager)
module H = Manager.H

let g = H.create ()

let v0 = "v0"
let v1 = "v1"
let v2 = "v2"
let v3 = "v3"
let v4 = "v4"
let v5 = "v5"
let v6 = "v6"
let v7 = "v7"
let v8 = "v8"
let v9 = "v9"
let v10 = "v10"

let vert = [v0;v1;v2;v3;v4;v5;v6; (*v7;v8;v9;v10*) ]

let () =
  List.iter (fun v -> H.add_vertex g v ()) vert;
  H.add_hedge g 01 () ~pred:[|v0|] ~succ:[|v1|];
  H.add_hedge g 12 () ~pred:[|v1|] ~succ:[|v2|];
  H.add_hedge g 23 () ~pred:[|v2|] ~succ:[|v3|];
  H.add_hedge g 26 () ~pred:[|v2|] ~succ:[|v6|];
  H.add_hedge g 34 () ~pred:[|v3|] ~succ:[|v4|];
  H.add_hedge g 45 () ~pred:[|v4|] ~succ:[|v5|];
  H.add_hedge g 52 () ~pred:[|v5|] ~succ:[|v2|]

  (* H.add_hedge g 01 () ~pred:[|v0|] ~succ:[|v1|]; *)
  (* H.add_hedge g 12 () ~pred:[|v1|] ~succ:[|v2|]; *)
  (* H.add_hedge g 23 () ~pred:[|v2|] ~succ:[|v3|]; *)
  (* H.add_hedge g 210 () ~pred:[|v2|] ~succ:[|v10|]; *)
  (* H.add_hedge g 34 () ~pred:[|v3|] ~succ:[|v4|]; *)
  (* H.add_hedge g 45 () ~pred:[|v4|] ~succ:[|v5|]; *)
  (* H.add_hedge g 59 () ~pred:[|v5|] ~succ:[|v9|]; *)
  (* H.add_hedge g 47 () ~pred:[|v4|] ~succ:[|v7|]; *)
  (* H.add_hedge g 79 () ~pred:[|v7|] ~succ:[|v9|]; *)
  (* H.add_hedge g 92 () ~pred:[|v9|] ~succ:[|v2|] *)

let r = FP.kleene_fixpoint g (Manager.H.VertexSet.singleton v0)

let print_attrvertex ppf vertex attr =
  A2.print () ppf attr

let print_attrhedge ppf hedge attr =
  Format.pp_print_int ppf hedge

let () =
  H.print_dot
    ~print_attrvertex
    ~print_attrhedge
    Format.std_formatter r
