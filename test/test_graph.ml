
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

  module VertexSet = Set.Make(Vertex)
  module HedgeSet = Set.Make(Hedge)
  module VertexTbl = Hashtbl.Make(Vertex)
  module HedgeTbl = Hashtbl.Make(Hedge)

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

let print_attrvertex ppf vertex attr =
  Format.pp_print_string ppf vertex

let print_attrhedge ppf hedge attr =
  Format.pp_print_int ppf hedge

let () =
  H.print_dot
    ~print_attrvertex
    ~print_attrhedge
    Format.std_formatter g
