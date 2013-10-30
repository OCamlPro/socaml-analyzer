open Common_types

val lambda_to_tlambda :
  modname : string ->
  funs: ( Common_types.F.t, Tlambda.tlambda * tid * tid * tid ) Hashtbl.t ->
  Lambda.lambda ->
  Tlambda.tlambda

(* for funs tids : arg, ret, exn *)
