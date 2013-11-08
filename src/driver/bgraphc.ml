open Common_types



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
  lam



let cmt_file ppf sourcefile outputprefix =
  let open Cmt_format in
  let cmtf = read_cmt sourcefile in
  let modulename = cmtf.cmt_modname in
  let cmtf = Cmt_specifics.restore_cmt_env cmtf in
  match cmtf.cmt_annots with
  | Implementation s ->
    Translmod.transl_implementation modulename (s,Typedtree.Tcoerce_none) (* Should be changed someday *)
  | _ -> failwith (Printf.sprintf "Bad cmt file: %s" sourcefile)

let handle_file ppf sourcefile =
  let outputprefix = Filename.chop_extension sourcefile in
  let modulename = String.capitalize (Filename.basename outputprefix) in

  let funs : ( F.t, Tlambda.tlambda ) Hashtbl.t =
    Hashtbl.create 1024 in

  let lambda =
    if Filename.check_suffix sourcefile ".ml"
    then ml_file ppf sourcefile outputprefix
    else if Filename.check_suffix sourcefile ".cmt"
    then cmt_file ppf sourcefile outputprefix
    else assert false
  in
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
    
