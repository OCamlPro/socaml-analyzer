open Hgraph_types

module MakeT ( V : OrderedHashedType ) ( H : OrderedHashedType )
  : T with type vertex = V.t and type hedge = H.t and module Vertex = V and module Hedge = H

module Make(T:T) : Hgraph
  with module T := T
   and type VertexSet.elt = T.vertex
   and type VertexSet.t = Set.Make(T.Vertex).t
   and module VertexSet = Set.Make(T.Vertex)
   and type VertexMap.key = T.vertex
   and type 'a VertexMap.t = 'a Map.Make(T.Vertex).t
   and module VertexMap = Map.Make(T.Vertex)
   and type VertexTbl.key = T.vertex
   and module VertexTbl = Hashtbl.Make(T.Vertex)
   and type HedgeSet.elt = T.hedge
   and module HedgeSet = Set.Make(T.Hedge)
   and type HedgeMap.key = T.hedge
   and module HedgeMap = Map.Make(T.Hedge)
   and type HedgeTbl.key = T.hedge
   and module HedgeTbl = Hashtbl.Make(T.Hedge)
