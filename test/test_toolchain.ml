let () =
  Clflags.nopervasives := true;
  Clflags.no_std_include := true

let lambdas = Mk_lambda.mk_lambdas ( Array.sub Sys.argv 1 ( pred ( Array.length Sys.argv ) ) )

let () =  print_endline "Got lambdas !"

let ir = ref (Mk_lambda.last_id () )
let mk_id () = 
  incr ir;
  Ident.({ name = ""; stamp = !ir; flags = 0 })

let funs : ( Common_types.F.t, Tlambda.tlambda ) Hashtbl.t = Hashtbl.create 1024

let tlambdas =
  Array.map
    (fun ( lam, modname ) ->
       Mk_tlambda.lambda_to_tlambda
         (* ~mk_id ~mk_fid:Common_types.F.create *) ~modname ~funs lam )
    lambdas

let () =  print_endline "Got Tlambdas !"

let _ = Array.fold_left (fun (_,env) -> Tlambda_interpret.tlambda funs env )
 (Tlambda_interpret.val_unit, Tlambda_interpret.env_empty) tlambdas
