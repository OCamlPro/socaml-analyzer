open Common_types

val lambda_to_tlambda :
  modname : string ->
  funs: ( Common_types.F.t, Tlambda.tlambda ) Hashtbl.t ->
  Lambda.lambda ->
  Tlambda.tlambda

