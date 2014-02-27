open Hgraph_types
open Stack_types

module OneLevel (T:OrderedHashedType) : Stack with type elt = T.t

module TwoLevels (T:OrderedHashedType) : Stack with type elt = T.t

module Local (Top:Stack) : Stack with type elt = Top.elt

module Leveled (N:N) (T:LeveledFunction) : Stack with type elt = T.t
