module Vertex : Hgraph.OrderedHashedType
module Hedge : Hgraph.OrderedHashedType

module G : ( Hgraph.Hgraph with type T.vertex = Vertex.t and type T.hedge = Hedge.t )

type id = Ident.t

type hinfo =
| Var of id
| Const of Lambda.structured_constant
| Prim of Tlambda.primitive * id list
| Constraint of constr
and constr = Ccp of int | Ctag of int

val mk_graph : last_id:int -> funs:Tlambda.tlambda array -> Tlambda.tlambda -> ( unit, ( id * hinfo ) list, unit ) G.graph * Vertex.t * Vertex.t * Vertex.t
(* the graph, the in, out and exn vectors *)
