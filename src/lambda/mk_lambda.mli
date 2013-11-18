val mk_lambdas : Format.formatter -> string array -> ( Lambda.lambda * string ) array

val mk_lambda : Format.formatter -> string -> Lambda.lambda * string
(* Takes .cmt, .cmti, .ml, .mli files as input and return a single lambda code from it and the last id stamp given. The produced lambda code is epurated from any global variable *)

