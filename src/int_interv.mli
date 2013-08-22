type t

val bottom : t
val top : t

val is_bottom : t -> bool
val is_top : t -> bool

val is_leq : t -> t -> bool
val join : t -> t -> t
val join_list : t list -> t

val widening : t -> t -> t
val print : Format.formatter -> t -> unit

val meet : t -> t -> t
val cst : int -> t
val addcst : t -> int -> t

val uminus : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val modulo : t -> t -> t
val band : t -> t -> t
val bor : t -> t -> t
val bxor : t -> t -> t
val blsl : t -> t -> t
val blsr : t -> t -> t
val basr : t -> t -> t

type compres = bool option

val comp : Lambda.comparison -> t -> t -> compres
val leqcst : t -> int -> t
val geqcst : t -> int -> t
