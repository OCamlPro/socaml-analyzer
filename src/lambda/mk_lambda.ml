exception Bad_file
exception No_implementation

let () =
  Clflags.nopervasives := true;
  Clflags.no_std_include := true


let ml_file ppf sourcefile outputprefix =
  Location.input_name := sourcefile;
  Compmisc.init_path true;
  let modulename =
    String.capitalize(Filename.basename(Misc.chop_extension_if_any sourcefile)) in
  (* check_unit_name ppf sourcefile modulename; *)
  Env.set_unit_name modulename;
  let inputfile = Pparse.preprocess sourcefile in
  let env = Compmisc.initial_env() in
  Compilenv.reset ?packname:!Clflags.for_package modulename;
  let lam =
    Pparse.file ppf inputfile Parse.implementation Config.ast_impl_magic_number
    |> Typemod.type_implementation sourcefile outputprefix modulename env
    |> Translmod.transl_implementation modulename
  in
  Pparse.remove_preprocessed inputfile;
  ( lam, modulename )

let cmt_file ppf sourcefile outputprefix =
  let open Cmt_format in
  let cmtf = read_cmt sourcefile in
  let modulename = cmtf.cmt_modname in
  let cmtf = Cmt_specifics.restore_cmt_env cmtf in
  match cmtf.cmt_annots with
  | Implementation s ->
    ( Translmod.transl_implementation
        modulename
        (s,Typedtree.Tcoerce_none) (* Should be changed someday *)
      , modulename )
  | _ -> failwith (Printf.sprintf "Bad cmt file: %s" sourcefile)

(* let add_cmt result ppf sourcefile outputprefix = *)
(*   Queue.add ( cmt_file ppf sourcefile outputprefix ) result *)
(* let add_cmti _ = () *)
(* let add_ml result ppf sourcefile outpoutprefix = *)
(*   Queue.add ( ml_file ppf sourcefile outputprefix ) result *)
(* let add_mli _ = () *)

(* let add_file result ppf sourcefile = *)
(*   let outputprefix = Filename.chop_extension sourcefile in *)
(*   let c = Filename.check_suffix sourcefile in *)
(*   if c ".cmt" *)
(*   then add_cmt result ppf sourcefile outputprefix *)
(*   else if c ".cmti" *)
(*   then add_cmti sourcefile *)
(*   else if c ".ml" *)
(*   then add_ml result ppf sourcefile outputprefix *)
(*   else if c ".mli" *)
(*   then add_mli sourcefile *)
(*   else raise Bad_file *)

let mk_lambda ppf sourcefile =
  let outputprefix = Filename.chop_extension sourcefile in
  let c = Filename.check_suffix sourcefile in
  if c ".cmt"
  then cmt_file ppf sourcefile outputprefix
  else if c ".ml"
  then ml_file ppf sourcefile outputprefix
  else raise Bad_file


let mk_lambdas ppf ( files : string array ) =
  Array.map ( mk_lambda ppf ) files

(* let unglobalize lambdas = *)
(*   let open Lambda in *)
(*   let m = *)
(* object (self) *)
(*   inherit Lmapper.mapper as super *)

(*   val mutable globals = ([] : ( Ident.t * lambda ) list ) *)
(*   method register_global i ll = *)
(*     globals <- (i,ll) :: globals *)

(*   method! prim p l = *)
(*     match p with *)
(*     | Pgetglobal id when not ( Common_types.builtin id ) -> self#var id *)
(*     | _ -> super#prim p l *)
(* end *)
(*   in *)
(*   let rec aux i = *)
(*     if i = pred (Array.length lambdas) *)
(*     then *)
(*       match m#lambda (fst lambdas.(i)) with *)
(*       | Lprim (Psetglobal id, ( [lam]) ) -> *)
(* 	m#register_global id lam; *)
(* 	lam *)
(*       | _ -> assert false *)
(*     else *)
(*       let l = m#lambda (fst lambdas.(i)) in *)
(*       match l with *)
(*       | Lprim (Psetglobal id, [lam]) -> *)
(* 	m#register_global id lam; *)
(* 	Llet ( Alias, id, lam, aux (succ i)) *)
(*       | _ -> assert false *)
(*   in *)
(*   aux 0 *)

(* let merge_lambdas ( lambdas : ( Lambda.lambda * string ) array ) = *)
(*   if lambdas = [| |] *)
(*   then raise No_implementation *)
(*   else unglobalize lambdas *)

