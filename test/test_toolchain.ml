let () =
  Clflags.nopervasives := true;
  Clflags.no_std_include := true

let lambdas =
  Mk_lambda.mk_lambdas
    Format.std_formatter
    ( Array.sub Sys.argv 1 ( pred ( Array.length Sys.argv ) ) )

let () =  print_endline "Got lambdas !"


let funs : ( Common_types.F.t, Tlambda.tlambda ) Hashtbl.t = Hashtbl.create 1024

let tlambdas =
  Array.map
    (fun ( lam, modname ) ->
       Mk_tlambda.lambda_to_tlambda
         ~modname ~funs lam )
    lambdas

let () =  print_endline "Got Tlambdas !"

let _ = Array.fold_left (fun (_,env) -> Tlambda_interpret.tlambda funs env )
 (Tlambda_interpret.val_unit, Tlambda_interpret.env_empty) tlambdas
