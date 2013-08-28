module Vertex : Hgraph.OrderedHashedType
module Hedge : Hgraph.OrderedHashedType

module G : ( Hgraph.Hgraph with type T.vertex = Vertex.t and type T.hedge = Hedge.t )

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
  }


val mk_graph : last_id:int -> funs:( int, Tlambda.tlambda ) Hashtbl.t -> Tlambda.tlambda -> ( unit, ( id * hinfo ) list, unit ) G.graph * Vertex.t * Vertex.t * Vertex.t * ( int, fun_desc ) Hashtbl.t
(* the graph, the in, out and exn vectors, the functions *)
