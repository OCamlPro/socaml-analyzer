val mk_lambdas : string array -> ( Lambda.lambda * string ) array

val mk_lambda : string array -> Lambda.lambda * int
(* Takes .cmt, .cmti, .ml, .mli files as input and return a single lambda code from it and the last id stamp given. The produced lambda code is epurated from any global variable *)

val last_id : unit -> int
