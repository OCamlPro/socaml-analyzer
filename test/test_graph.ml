
module Vertex = struct
  type t = string
  let compare (i:string) j = Pervasives.compare i j
  let hash (i:string) = Hashtbl.hash i
  let equal (i:string) j = i = j

  let print ppf s = Format.pp_print_string ppf s

  let c = ref 0
  let clone v = Printf.sprintf "%s_%i" v (incr c; !c)
end

module Hedge = struct
  type t = int
  let compare (i:int) j = Pervasives.compare i j
  let hash i = Hashtbl.hash i
  let equal (i:int) j = i = j

  let print ppf i = Format.pp_print_int ppf i

  let counter = ref (-1)
  let new_hedge () = incr counter; !counter
  let clone _ = new_hedge ()
end

module T = struct

  type vertex = Vertex.t
  type hedge = Hedge.t
  module Vertex = Vertex
  module Hedge = Hedge

  let print_vertex = Vertex.print
  let print_hedge = Hedge.print

end

module H = Hgraph.Make(T)

let g = H.create ()

let v1 = "v1"
let v2 = "v2"
let v3 = "v3"
let v4 = "v4"

let h0 = Hedge.new_hedge ()
let h1 = Hedge.new_hedge ()
let h2 = Hedge.new_hedge ()

let () =
  H.add_vertex g v1 ();
  H.add_vertex g v2 ();
  H.add_vertex g v3 ();
  H.add_vertex g v4 ();
  H.add_hedge g h0 () [|v1|] [|v2|];
  H.add_hedge g h1 () [|v2;v3|] [|v4|];
  H.add_hedge g h2 () [|v4|] [|v3|]

let () = assert(H.correct g)

let g2 = H.create ()

let v1' = "v1_"
let v4' = "v4_"
let v5 = "v5"

let h3 = Hedge.new_hedge ()
let h4 = Hedge.new_hedge ()

let () =
  H.add_vertex g2 v1' ();
  H.add_vertex g2 v4' ();
  H.add_vertex g2 v5 ();
  H.add_hedge g2 h3 () [|v1'|] [|v5|];
  H.add_hedge g2 h4 () [|v5|] [|v4'|]

let () = assert(H.correct g2)

let vset l = List.fold_right H.VertexSet.add l H.VertexSet.empty
let hset l = List.fold_right H.HedgeSet.add l H.HedgeSet.empty

let subgraph =
  { H.sg_input = [|v1'|];
    sg_output = [|v4'|];
    sg_vertex = vset [v5];
    sg_hedge = hset [h3;h4] }

let subgraph = H.clone_subgraph
    ~in_graph:g2
    ~out_graph:g
    ~import_vattr:(fun ~old_vertex:_ ~new_vertex ~old_attr -> old_attr)
    ~import_hattr:(fun ~old_hedge:_ ~new_hedge ~old_attr -> old_attr)
    ~clone_vertex:(fun i -> i)
    ~clone_hedge:(fun i -> i)
    ~input:[|v1|]
    ~output:[|v4|]
    subgraph

let () = assert(H.correct g)
let () = assert(H.correct g2)

let print_attrvertex ppf vertex attr =
  Format.pp_print_string ppf vertex

let print_attrhedge ppf hedge attr =
  Format.pp_print_int ppf hedge

let () =
  H.print_dot
    ~print_attrvertex
    ~print_attrhedge
    Format.std_formatter g
