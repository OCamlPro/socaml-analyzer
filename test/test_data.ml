open Format
open Data

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

module Env = struct

  type t = Data.environment

  let any_int = { bottom with int = Top }

  let set_int env dst cst =
    set_env dst (int_singleton cst) env

  let add_int env ~src1 ~src2 ~dst =
    env
    |> set_env src1 (intersection (get_env src1 env) any_int)
    |> set_env src2 (intersection (get_env src2 env) any_int)
    |> set_env dst any_int

  let bottom = bottom_env
  let empty = empty_env

end

module Manager = struct
  type vertex = T.vertex
  type hedge = T.hedge
  type abstract = Data.environment

  type hedge_attribute = unit

  let a_id = Id.create ~name:"a" ()
  let b_id = Id.create ~name:"b" ()
  let c_id = Id.create ~name:"c" ()

  let n_cst = Constant.create ~name:"n" ()
  let m_cst = Constant.create ~name:"m" ()

  let apply hedge () tabs =
    let abs = tabs.(0) in
    let nabs =
      match hedge with
      | 01 -> Env.set_int abs a_id n_cst (* a <- n *)
      | 12 -> Env.set_int abs b_id m_cst (* b <- m *)
      | 23 -> Env.add_int abs a_id b_id c_id (* c <- a + b *)
      | _ -> failwith ""
    in
    nabs, []

  let abstract_init i =
    if i = "v0"
    then Env.empty
    else Env.bottom

  let bottom _ = bottom_env
  let is_bottom _ = is_bottom_env
  let join_list _ l = failwith "TODO"
  let is_leq _ x y = failwith "TODO"

end

module FP = Hgraph.Fixpoint(H)(Manager)

let g = H.create ()

let v0 = "v0"
let v1 = "v1"
let v2 = "v2"
let v3 = "v3"

let vert = [v0;v1;v2;v3]

let () =
  List.iter (fun v -> H.add_vertex g v ()) vert;
  H.add_hedge g 01 () ~pred:[|v0|] ~succ:[|v1|];
  H.add_hedge g 12 () ~pred:[|v1|] ~succ:[|v2|];
  H.add_hedge g 23 () ~pred:[|v2|] ~succ:[|v3|]

let r = FP.kleene_fixpoint g (H.VertexSet.singleton v0)
