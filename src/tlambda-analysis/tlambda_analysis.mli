type v = Tlambda_to_hgraph.Vertex.t

module type Entry =
sig
  val inv : v
  val outv : v
  val exnv : v
end

module M : functor ( Entry ) ->
  ( Hgraph.Manager with module H = Tlambda.to_hgraph.G )
