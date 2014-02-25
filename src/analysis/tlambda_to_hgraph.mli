open Common_types

module Vertex :
sig
  include Hgraph_types.OrderedHashedType
  val mk : ?modulename : string -> unit -> t
end
module Hedge :
sig
  include Hgraph_types.OrderedHashedType
  val mk : unit -> t
end

module T :  Hgraph_types.T
with type vertex = Vertex.t and type hedge = Hedge.t
and module Vertex = Vertex and module Hedge = Hedge

module G :
  ( Hgraph_types.Hgraph
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
    f_graph : ( unit, ( tid * hinfo ) list, unit ) G.graph;
    f_in : Vertex.t array;
    f_out : Vertex.t array;
    f_vertex : VertexSet.t;
    f_hedge : HedgeSet.t;
    (* f_arg : tid; *)
    (* f_return : tid; *)
    (* f_exn : tid; *)
  }

type mod_desc

type vattr = unit
type hattr = ( tid * hinfo ) list
type gattr = unit
type hg = ( vattr, hattr, gattr ) G.graph

 val mk_graph :
   modulename : string ->
   ( F.t, Tlambda.tlambda ) Hashtbl.t ->
   Tlambda.tlambda ->
   ( hg * ( F.t, fun_desc ) Hashtbl.t *
       Vertex.t * Vertex.t * Vertex.t *
       tid * tid )
(* the graph, the in, out and exn vectors, the functions, the exn and return tids *)


val init :
  modulename : string ->
  ( Data.f, Tlambda.tlambda ) Hashtbl.t ->
  hg * ( Data.f, fun_desc ) Hashtbl.t * tid

(*
   takes the last tid number and the fun hashtbl
   returns the graph, the fun descriptors and a exn_id
 *)

val mk_subgraph :
  g : hg ->
  modulename : string ->
  exn_id : tid ->
  Tlambda.tlambda ->
  mod_desc

(* returns a inv, a outv, a exnv and a return tid *)

val merge_graphs :
  g : hg ->
  mod_desc array ->
  ( Vertex.t * Vertex.t * Vertex.t * tid )
