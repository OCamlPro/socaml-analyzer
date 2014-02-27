
module type Stack = sig

  include Hgraph_types.OrderedHashedType

  type elt

  val empty : t
  val push : t -> elt -> t

end

module type LeveledFunction = sig
  include Hgraph_types.OrderedHashedType
  val is_important : t -> bool
end

module type N = sig
  val n : int
end
