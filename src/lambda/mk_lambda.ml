exception Bad_file
exception No_implementation

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
  ( Translmod.transl_implementation name
    ( tree, Typedtree.Tcoerce_none),
    name )

let mk_lambdas ( files : string array ) =
  let result = Queue.create () in
  Array.iter ( add_file result ) files;
  Array.init
    ( Queue.length result )
    ( fun _ -> tt_to_lambda ( Queue.take result ) )

let unglobalize lambdas =
  let open Lambda in
  let m =
object (self)
  inherit Lmapper.mapper as super

  val mutable globals = ([] : ( Ident.t * lambda ) list )
  method register_global i ll =
    globals <- (i,ll) :: globals

  method! prim p l =
    match p with
    | Pgetglobal id when not ( Common_types.builtin id ) -> self#var id
    | _ -> super#prim p l
end
  in
  let rec aux i =
    if i = pred (Array.length lambdas)
    then
      match m#lambda (fst lambdas.(i)) with
      | Lprim (Psetglobal id, ( [lam]) ) ->
	m#register_global id lam;
	lam
      | _ -> assert false
    else
      let l = m#lambda (fst lambdas.(i)) in
      match l with
      | Lprim (Psetglobal id, [lam]) ->
	m#register_global id lam;
	Llet ( Alias, id, lam, aux (succ i))
      | _ -> assert false
  in
  aux 0

let merge_lambdas ( lambdas : ( Lambda.lambda * string ) array ) =
  if lambdas = [| |]
  then raise No_implementation
  else unglobalize lambdas

let mk_lambda files = 
  let l = merge_lambdas ( mk_lambdas files ) in
  ( l, Tt_restore.last_ident () )

let last_id = Tt_restore.last_ident
