open Common_types


let handle_file ppf sourcefile =
  let outputprefix = Filename.chop_extension sourcefile in

  let funs : ( F.t, Tlambda.tlambda ) Hashtbl.t =
    Hashtbl.create 1024 in

  let lambda, modulename = Mk_lambda.mk_lambda ppf sourcefile in

  let tlambda =
    Mk_tlambda.lambda_to_tlambda
      ~modname:modulename ~funs
      lambda
  in
      
  let (g,funtbl,vin,vout,vexn,exn_id,return_id) =
    Tlambda_to_hgraph.mk_graph ~modulename funs tlambda
  in

  Cmb.export g funtbl vin vout vexn outputprefix

let arg_parser =
  let open Arg in
  [
    ( "-open",
     String
       (fun s ->
          Compenv.implicit_modules := 
            s :: !Compenv.implicit_modules
       ),
     "Add an implicitly opened module" )
  ]

let () =

  Clflags.nopervasives := true;
  Clflags.no_std_include := true;

  let ppf = Format.std_formatter in

  Arg.parse arg_parser (handle_file ppf)
    "please specify your .ml or .cmt files, order matters";
    
