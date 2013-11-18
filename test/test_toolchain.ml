let arg_parser =
  let open Arg in
  [
    ( "-open",
      String Mk_lambda.open_module,
      "Add an implicitly opened module" )
  ]

let lamq = Queue.create ()

let () = Arg.parse
    arg_parser
    (fun sourcefile ->
       let lm = Mk_lambda.mk_lambda Format.std_formatter sourcefile in
       Queue.add lm lamq
    )
    "please specify your .ml or .cmt files, order matters"

let lambdas =
  Array.init ( Queue.length lamq ) (fun _ -> Queue.pop lamq)

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
