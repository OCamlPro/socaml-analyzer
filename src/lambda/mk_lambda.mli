val mk_lambdas : Format.formatter -> string array -> ( Lambda.lambda * string ) array

val mk_lambda : Format.formatter -> string -> Lambda.lambda * string
(* Takes .cmt, .cmti, .ml, .mli files as input and return a single lambda code from it and the last id stamp given. The produced lambda code is epurated from any global variable *)

val ml_file : Format.formatter -> string -> string -> Lambda.lambda * string
val mli_file : Format.formatter -> string -> string -> unit
val cmt_file : Format.formatter -> string -> string -> Lambda.lambda * string
val cmti_file : Format.formatter -> string -> string -> unit

val open_module : string -> unit
val close_module : string -> unit
