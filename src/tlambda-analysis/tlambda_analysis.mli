open Common_types
open Tlambda_to_hgraph

type v = Vertex.t

module type Entry =
sig
  val inv : v
  val outv : v
  val exnv : v
  val g : ( unit, ( tid * hinfo ) list, unit ) G.graph 
  val funs : ( Data.f, fun_desc ) Hashtbl.t
  val mk_vertex : unit -> v
  val mk_hedge : unit -> Hedge.t
end

module M : functor ( E : Entry ) ->
   Hgraph.Manager
  with
    module T := T
  and module H = G
