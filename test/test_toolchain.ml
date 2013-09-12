let lambda, last_id = Mk_lambda.mk_lambda ( Array.sub Sys.argv 1 ( pred ( Array.length Sys.argv ) ) )
let last_id, funs, tlambda =
  Mk_tlambda.lambda_to_tlambda last_id lambda

let _ = Tlambda_interpret.tlambda funs Tlambda_interpret.env_empty tlambda