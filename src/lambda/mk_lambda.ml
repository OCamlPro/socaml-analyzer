exception Bad_file

let add_cmt result file =
  Queue.add ( Tt_restore.load_and_restore file ) result
let add_cmti = Tt_restore.add_interface
let add_ml result file = Queue.add ( Tt_restore.load_ml file ) result
let add_mli = Tt_restore.load_mli

let add_file result file =
  let c = Filename.check_suffix file in
  if c ".cmt"
  then add_cmt result file
  else if c ".cmti"
  then add_cmti file
  else if c ".ml"
  then add_ml result file
  else if c ".mli"
  then add_mli file
  else raise Bad_file

let tt_to_lambda ( name, tree ) =
  Translmod.transl_implementation name
    ( tree, Typedtree.Tcoerce_none)

let mk_lambda ( files : string array ) =
  let result = Queue.create () in
  Array.iter ( add_file result ) files;
  Array.init
    ( Queue.length result )
    ( fun _ -> tt_to_lambda ( Queue.take result ) )
