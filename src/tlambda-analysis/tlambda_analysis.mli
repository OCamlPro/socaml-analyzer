type v = Tlambda_to_hgraph.Vertex.t

module type Entry =
sig
  val inv : v
  val outv : v
  val exnv : v
end

module M : functor ( E : Entry ) ->
   Hgraph.Manager with module H = Tlambda_to_hgraph.G
