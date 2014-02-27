
module type Stack = sig

  include Hgraph_types.OrderedHashedType

  type elt

  val empty : t
  val push : t -> elt -> t

end
