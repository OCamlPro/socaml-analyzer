open Format

module A1 = struct
  type t = (int * int) option
  type manager = unit

  let bottom man = None
  let top man = Some(min_int,max_int)
  let canonical (x:t) = match x with
    | Some(a,b) when a>b -> None
    | _ -> x

  let is_bottom man (x:t) =
    x=None
  let is_top man (x:t) =
    match x with
    | None -> false
    | Some(a,b) when a>min_int || b<max_int -> false
    | _ -> true
  let is_leq man (x:t) y =
    match (x,y) with
    | (None,_) -> true
    | (Some _, None) -> false
    | (Some(a1,b1),Some(a2,b2)) ->
      let r = a1>=a2 && b1<=b2 in
      (* Printf.printf "leq : [%i,%i] <= [%i,%i] : %b\n%!" a1 b1 a2 b2 r; *)
      r

  let join man (x:t) y =
    match (x,y) with
    | (None,_) -> y
    | (_,None) -> x
    | (Some(a1,b1),Some(a2,b2)) ->
      canonical (Some((min a1 a2),(max b1 b2)))

  let join_list man tx =
    List.fold_left (fun res x ->
        join man res x) (bottom man) tx
  let widening man (x:t) y =
    match (x,y) with
    | (None,_) -> y
    | (_,None) -> failwith ""
    | (Some(a1,b1),Some(a2,b2)) ->
      canonical
        (Some(
            (if a2<a1 then min_int else a2),
            (if b2>b1 then max_int else b2)))

  let narrow man (x:t) y =
    match (x,y) with
    | (_, None) -> x
    | (None,_) -> failwith ""
    | (Some(a1,b1),Some(a2,b2)) ->
      canonical (Some(
          (if a1 = min_int then a2 else a1),
          (if b1 = max_int then b2 else b1)))

  let print man fmt (x:t) =
    match x with
    | Some(a,b) ->
      if is_top man x then pp_print_string fmt "top"
      else
        let s i =
          if i = min_int
          then "-∞"
          else if i = max_int
          then "∞"
          else string_of_int i
        in
        fprintf fmt "[%s,%s]" (s a) (s b)
    | None -> pp_print_string fmt "bot"

  let meet man (x:t) y =
    match (x,y) with
    | (None,_) -> None
    | (_,None) -> None
    | (Some(a1,b1),Some(a2,b2)) ->
      let a = max a1 a2 in
      let b = min b1 b2 in
      canonical (if a>b then None else Some(a,b))

  let setcst man (x:t) (cst:int) =
    match x with
    | None -> x
    | Some(a,b) -> canonical (Some(cst,cst))

  let addcst man (x:t) (cst:int) =
    match x with
    | None -> x
    | Some(a,b) ->
      canonical (Some(
          (if a=min_int then a else a+cst),
          (if b=max_int then b else b+cst)))

  let mulcst man (x:t) (cst:int) =
    match x with
    | None -> x
    | Some(a,b) ->
      if cst = 0
      then Some (0,0)
      else if cst > 0
      then
        canonical (Some(
            (if a=min_int then a else a * cst),
            (if b=max_int then b else b * cst)))
      else
        canonical (Some(
            (if b=max_int then min_int else b * cst),
            (if a=min_int then max_int else a * cst)))

  let leqcst man (x:t) (cst:int) =
    match x with
    | None -> x
    | Some(a,b) ->
      let i = canonical (Some(min_int,cst)) in
      meet man (x:t) i

  let geqcst man (x:t) (cst:int) =
    match x with
    | None -> x
    | Some(a,b) ->
      let i = canonical (Some(cst,max_int)) in
      meet man x i

  let leq man (x:t) (y:t) =
    match x, y with
    | None, _ | _, None -> None, None
    | Some(x1,x2), Some(y1,y2)->
      canonical (Some(x1,min x2 y2)),
      canonical (Some (max x1 y1, y2))

  let less man (x:t) (y:t) =
    match x, y with
    | None, _ | _, None -> None, None
    | Some(x1,x2), Some(y1,y2)->
      let add i n = if i = max_int || i = min_int then i else i + n in
      canonical (Some(x1,min x2 (add y2 (-1)))),
      canonical (Some (max (add x1 1) y1, y2))

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
  let is_leq man (x1,y1) (x2,y2) =
    A1.is_leq man x1 x2 && A1.is_leq man y1 y2
  let join man (x1,y1) (x2,y2) =
    (A1.join man x1 x2, A1.join man y1 y2)
  let join_list man tx =
    List.fold_left (fun res x ->
        join man res x) (bottom man) tx
  let widening man (x1,y1) (x2,y2) =
    (A1.widening man x1 x2, A1.widening man y1 y2)
  let narrow man (x1,y1) (x2,y2) =
    (A1.narrow man x1 x2, A1.narrow man y1 y2)
  let narrowing =
    (* None *)
    Some narrow
  let print man fmt ((x,y) as v) =
    if is_bottom man v then pp_print_string fmt "bot"
    else fprintf fmt "(%a,%a)" (A1.print man) x (A1.print man) y
  let meet man (x1,y1) (x2,y2) =
    (A1.meet man x1 x2, A1.meet man y1 y2)
  let setcst man ((x,y) as v) (dim:int) (cst:int) =
    if is_bottom man v
    then v
    else if dim=0 then (A1.setcst man x cst, y)
    else if dim=1 then (x, A1.setcst man y cst)
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
    else if dim=0 then (A1.addcst man x cst, y)
    else if dim=1 then (x, A1.addcst man y cst)
    else failwith ""

  let mulcst man ((x,y) as v) (dim:int) (cst:int) =
    if is_bottom man v
    then v
    else if dim=0 then (A1.mulcst man x cst, y)
    else if dim=1 then (x, A1.mulcst man y cst)
    else failwith ""

  let leqcst man ((x,y) as v) (dim:int) (cst:int) =
    if is_bottom man v
    then v
    else if dim=0 then (A1.leqcst man x cst, y)
    else if dim=1 then (x, A1.leqcst man y cst)
    else failwith ""
  let geqcst man ((x,y) as v) (dim:int) (cst:int) =
    if is_bottom man v
    then v
    else if dim=0 then (A1.geqcst man x cst, y)
    else if dim=1 then (x, A1.geqcst man y cst)
    else failwith ""

  let x_leq_y man ((x,y):t) =
    A1.leq man x y

  let y_leq_x man ((x,y):t) =
    A1.leq man y x

  let x_less_y man ((x,y):t) =
    A1.less man x y

  let y_less_x man ((x,y):t) =
    A1.less man y x

end

module Vertex = struct
  type t = string
  let compare (i:string) j = Pervasives.compare i j
  let hash (i:string) = Hashtbl.hash i
  let equal (i:string) j = i = j

  let print ppf s = Format.pp_print_string ppf s
  let counter = ref 0
  let clone v = incr counter; v ^ "_" ^ (string_of_int !counter)
end

module Hedge = struct
  type t = int
  let compare (i:int) j = Pervasives.compare i j
  let hash i = Hashtbl.hash i
  let equal (i:int) j = i = j

  let print ppf i = Format.pp_print_int ppf i

  let counter = ref (99)
  let new_hedge () = incr counter; !counter
  let clone i = new_hedge ()
end

module T = struct

  type vertex = Vertex.t
  type hedge = Hedge.t
  module Vertex = Vertex
  module Hedge = Hedge

  let print_vertex = Vertex.print
  let print_hedge = Hedge.print

end

module Manager_base = struct
  module H = Hgraph.Make(T)
  include A2
  type abstract = A2.t

  type vertex_attribute = unit
  type hedge_attribute = string
  type graph_attribute = unit

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
    let print _ _ = assert false
  end

  let find_function _ = assert false
  module Stack = Abstract_stack.TwoLevels ( Function_id )

end

let print_attrvertex ppf vertex attr =
  A2.print () ppf attr

let print_attrhedge ppf hedge attr =
  Format.pp_print_string ppf attr

module Test1 = struct
  module Manager = struct
    include Manager_base
    let apply hedge attr tabs =
      let abs = tabs.(0) in
      let nabs =
        match attr with
        | "01" -> A2.setcst () abs 0 0
        | "12" -> A2.setcst () abs 1 0
        | "23" -> A2.leqcst () abs 0 99
        | "26" -> A2.geqcst () abs 0 100
        | "34" -> A2.addcst () abs 0 1
        | "45" -> A2.assignvar () abs 1 0
        | "52" -> abs
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
  end

  module FP = Fixpoint.Fixpoint(T)(Manager)
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

  let vert = [v0;v1;v2;v3;v4;v5;v6;]

  let () =
    List.iter (fun v -> H.add_vertex g v ()) vert;
    H.add_hedge g 01 "01" ~pred:[|v0|] ~succ:[|v1|];
    H.add_hedge g 12 "12" ~pred:[|v1|] ~succ:[|v2|];
    H.add_hedge g 23 "23" ~pred:[|v2|] ~succ:[|v3|];
    H.add_hedge g 26 "26" ~pred:[|v2|] ~succ:[|v6|];
    H.add_hedge g 34 "34" ~pred:[|v3|] ~succ:[|v4|];
    H.add_hedge g 45 "45" ~pred:[|v4|] ~succ:[|v5|];
    H.add_hedge g 52 "52" ~pred:[|v5|] ~succ:[|v2|]

  let r, map = FP.kleene_fixpoint g (Manager.H.VertexSet.singleton v0)

  (* let () = *)
  (*   H.print_dot *)
  (*     ~print_attrvertex *)
  (*     ~print_attrhedge *)
  (*     Format.std_formatter r *)

end

module Test2 = struct
  module Manager = struct
    include Manager_base
    let apply hedge attr tabs =
      let abs = tabs.(0) in
      let nabs =
        match attr with
        | "x := 1" -> A2.setcst () abs 0 1
        | "y <= 10" -> A2.leqcst () abs 1 10
        | "y > 10" -> A2.geqcst () abs 1 11
        | "x < y" -> A2.x_less_y () abs
        | "x >= y"-> A2.y_leq_x () abs
        | "y := 10"-> A2.setcst () abs 1 10
        | "x := y"->A2.assignvar () abs 0 1
        | "x := x + 1"->A2.addcst () abs 0 1
        | "x := x * 2" -> A2.mulcst () abs 0 2
        | "y := y - 1" -> A2.addcst () abs 1 (-1)
        | _ -> failwith ""
      in
      [|nabs|], []
  end

  module FP = Fixpoint.Fixpoint(T)(Manager)
  module H = Manager.H

  let g = H.create ()

  let v0 = "v0"
  let v2 = "v2"
  let v3 = "v3"
  let v6 = "v6"
  let v7 = "v7"
  let v8 = "v8"
  let v11 = "v11"
  let v12 = "v12"
  let v13 = "v13"

  let vert = [v0;v2;v3;v6;v7;v8;v11;v12;v13]

  let () =
    List.iter (fun v -> H.add_vertex g v ()) vert;
    H.add_hedge g 0_2 "x := 1" ~pred:[|v0|] ~succ:[|v2|];
    H.add_hedge g 2_3 "y <= 10" ~pred:[|v2|] ~succ:[|v3|];
    H.add_hedge g 2_6 "y > 10" ~pred:[|v2|] ~succ:[|v6|];
    H.add_hedge g 6_7 "x < y" ~pred:[|v6|] ~succ:[|v7|];
    H.add_hedge g 6_11 "x >= y" ~pred:[|v6|] ~succ:[|v11|];
    H.add_hedge g 3_11 "y := 10" ~pred:[|v3|] ~succ:[|v11|];
    H.add_hedge g 11_12 "x := y" ~pred:[|v11|] ~succ:[|v12|];
    H.add_hedge g 12_13 "x := x + 1" ~pred:[|v12|] ~succ:[|v13|];
    H.add_hedge g 7_8 "x := x * 2" ~pred:[|v7|] ~succ:[|v8|];
    H.add_hedge g 8_6 "y := y - 1" ~pred:[|v8|] ~succ:[|v6|]

  let r, map = FP.kleene_fixpoint g (Manager.H.VertexSet.singleton v0)

  let () =
    H.print_dot
      ~print_attrvertex
      ~print_attrhedge
      Format.std_formatter r
end
