module Vertex :
sig
  include Hgraph.OrderedHashedType
  val mk : unit -> t
end
module Hedge :
sig
  include Hgraph.OrderedHashedType
  val mk : unit -> t
end

module T :  Hgraph.T
with type vertex = Vertex.t and type hedge = Hedge.t
and module Vertex = Vertex and module Hedge = Hedge

module G :
  ( Hgraph.Hgraph
    with
      type T.vertex = T.vertex
    and type T.hedge = T.hedge
    and module T := T
    and type VertexSet.t = Set.Make(T.Vertex).t
    and type VertexSet.elt = T.vertex
    and type HedgeSet.t = Set.Make(T.Hedge).t
    and type HedgeSet.elt = Set.Make(T.Hedge).elt
    and module VertexSet = Set.Make(T.Vertex)
    and module HedgeSet = Set.Make(T.Hedge)
  )
    
open G

type id = Ident.t

type hinfo =
| Var of id
| Const of Lambda.structured_constant
| Prim of Tlambda.primitive * id list
| Constraint of constr
| App of id * id (* function, argument *)
and constr = Ccp of int | Ctag of int

type fun_desc =
  {
    f_graph : ( unit, (id * hinfo) list, unit ) G.graph;
    f_in : Vertex.t array;
    f_out : Vertex.t array;
    f_vertex : VertexSet.t;
    f_hedge : HedgeSet.t;
  }


val mk_graph : last_id:int -> funs:( Data.f, Tlambda.tlambda ) Hashtbl.t -> Tlambda.tlambda -> ( unit, ( id * hinfo ) list, unit ) G.graph * Vertex.t * Vertex.t * Vertex.t * ( Data.f, fun_desc ) Hashtbl.t * id * id * id
(* the graph, the in, out and exn vectors, the functions, the arg, return and exn ids *)
