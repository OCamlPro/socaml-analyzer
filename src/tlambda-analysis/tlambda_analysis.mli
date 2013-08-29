open Tlambda_to_hgraph

type v = Vertex.t

module type Entry =
sig
  val inv : v
  val outv : v
  val exnv : v
  val g : ( unit, ( id * hinfo ) list, unit ) G.graph 
  val funs : ( Data.f, fun_desc ) Hashtbl.t
  val mk_vertex : unit -> v
  val mk_hedge : unit -> Hedge.t
  val return_id : Ident.t

end

module M : functor ( E : Entry ) ->
   Hgraph.Manager
  with
    module T := T
  and module H = G
