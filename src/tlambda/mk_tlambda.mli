open Common_types

val lambda_to_tlambda :
  mk_id:(unit -> id) -> mk_fid:(unit -> F.t) ->
  funs: ( Common_types.F.t, Tlambda.tlambda ) Hashtbl.t ->
  Lambda.lambda -> Tlambda.tlambda
