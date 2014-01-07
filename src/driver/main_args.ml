open Arg

(* let ppf = Format.std_formatter in *)

let cmbise outputprefix = outputprefix ^ ".cmb"
let targets = ref []
let add_target t = targets := t :: !targets

let get_targets () = List.rev !targets

let arg_parser =
  let open Arg in
  [
    ( "-open",
      String Mk_lambda.open_module,
      "Add an implicitly opened module" );
    ( "-close",
      String Mk_lambda.close_module,
      "Remove an implicitly opened module" );
  ]

let handle_file ppf sourcefile =

  let outputprefix = Filename.chop_extension sourcefile in
  
  let c = Filename.check_suffix sourcefile in

  if c ".ml"
  then
    begin
      Mk_lambda.ml_file ppf sourcefile outputprefix;
      add_target (cmbise outputprefix)
    end
  else if c ".cmt"
  then
    begin
      Mk_lambda.cmt_file ppf sourcefile outputprefix;
      add_target (cmbise outputprefix)
    end      
  else if c ".mli"
  then Mk_lambda.mli_file ppf sourcefile outputprefix
  else if c ".cmti"
  then Mk_lambda.cmti_file ppf sourcefile outputprefix
  else if c ".cmb"
  then add_target sourcefile
  else failwith ("bad file suffix for file " ^ sourcefile )
