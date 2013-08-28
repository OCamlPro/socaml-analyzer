open Tlambda_to_hgraph

type v = Vertex.t

module type Entry =
sig
  val inv : v
  val outv : v
  val exnv : v
  val g : ( unit, ( id * hinfo ) list, unit ) G.graph 
  val funs : ( int, fun_desc ) Hashtbl.t
end

module M : functor ( E : Entry ) ->
   Hgraph.Manager with module H = G
