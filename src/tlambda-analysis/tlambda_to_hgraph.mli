open Common_types

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

type fun_desc =
  {
    f_graph : ( unit, (id * hinfo) list, unit ) G.graph;
    f_in : Vertex.t array;
    f_out : Vertex.t array;
    f_vertex : VertexSet.t;
    f_hedge : HedgeSet.t;
    f_arg : id;
    f_return : id;
    f_exn : id;
  }

type hg = ( unit, ( id * hinfo ) list, unit ) G.graph

(* val mk_graph : last_id:int -> funs:( Data.f, Tlambda.tlambda ) Hashtbl.t -> Tlambda.tlambda -> ( unit, ( id * hinfo ) list, unit ) G.graph * Vertex.t * Vertex.t * Vertex.t * ( Data.f, fun_desc ) Hashtbl.t * id * id * id *)
(* (\* the graph, the in, out and exn vectors, the functions, the arg, return and exn ids *\) *)


val init :
  last_id:int ->
  ( Data.f, Tlambda.tlambda ) Hashtbl.t ->
  hg * ( Data.f, fun_desc ) Hashtbl.t * id

(*
   takes the last id number and the fun hashtbl
   returns the graph, the fun descriptors and a exn_id
 *)

val mk_subgraph :
  g : hg -> exn_id : id -> Tlambda.tlambda ->
  ( Vertex.t * Vertex.t * Vertex.t * id )

(* returns a inv, a outv, a exnv and a return id *)

val merge_graphs :
  g : hg ->
  ( Vertex.t * Vertex.t * Vertex.t * id ) array ->
  ( Vertex.t * Vertex.t * Vertex.t * id )
