open Cmt_format
open Tt_mapper
open Ident

module Sm = Map.Make ( struct type t = string let compare = compare end )

exception Unspecified_module of string

class restorer =
object (self)

  inherit mapper

  val mutable import = Sm.empty
  val mutable last_env = Env.initial
  val env_cache =
    (Hashtbl.create 59 : ((Env.summary * Subst.t), Env.t) Hashtbl.t)

  method! env e = Env.env_of_only_summary self#env_from_summary e

  method reset_cache =
    Hashtbl.clear env_cache;
    Env.reset_cache()

  method private extract_sig env mty =
    match Mtype.scrape env mty with
      Types.Mty_signature sg -> sg
    | _ -> assert false

  method add_module_env i m =
    (* Printf.printf "Adding %s with id %d\n%!" i.Ident.name i.Ident.stamp; *)
    last_env <- Env.add_module i m last_env
  method add_modtype_env i m =
    last_env <- Env.add_modtype i m last_env



  method env_from_summary sum subst =
    try
      Hashtbl.find env_cache (sum, subst)
    with Not_found ->
      let env =
	let open Env in
	match sum with (* tester si id < 1000 et mettre last_env *)
	| Env_value (_,id,_) | Env_type( _, id, _) | Env_exception(_, id, _) | Env_module(_, id, _) | Env_modtype(_, id, _) | Env_class(_, id, _) | Env_cltype (_, id, _)
	  when id.stamp < 1000 -> last_env
        | Env_empty -> last_env
	| Env_value(s, id, desc) ->
          Env.add_value (self#ident id) (Subst.value_description subst ( self#t_value_description desc)) (self#env_from_summary s subst)
	| Env_type(s, id, desc) ->
          Env.add_type (self#ident id) (Subst.type_declaration subst ( self#t_type_declaration desc)) (self#env_from_summary s subst)
	| Env_exception(s, id, desc) ->
          Env.add_exception (self#ident id) (Subst.exception_declaration subst ( self#t_exception_declaration desc)) (self#env_from_summary s subst)
	| Env_module(s, id, desc) ->
          Env.add_module (self#ident id) (Subst.modtype subst (self#t_module_type desc)) (self#env_from_summary s subst)
	| Env_modtype(s, id, desc) ->
          Env.add_modtype (self#ident id) (Subst.modtype_declaration subst (self#t_modtype_declaration desc)) (self#env_from_summary s subst)
	| Env_class(s, id, desc) ->
          Env.add_class (self#ident id) (Subst.class_declaration subst (self#t_class_declaration desc)) (self#env_from_summary s subst)
	| Env_cltype (s, id, desc) ->
          Env.add_cltype (self#ident id) (Subst.cltype_declaration subst (self#t_class_type_declaration desc)) (self#env_from_summary s subst)
	| Env_open(s, path) ->
          let env = self#env_from_summary s subst in
          let path' = Subst.module_path subst (self#path path) in
          let mty =
            try Env.find_module path' env with Not_found ->
	      (
		try
		  begin
		    match Env.find_modtype path' env with
		    | Types.Modtype_manifest m -> m
		    | _ -> raise Not_found
		  end
		with Not_found ->
		  raise
		    (Unspecified_module
		       (match path' with
			 Path.Pident i -> Printf.sprintf "%d: %s" i.Ident.stamp i.Ident.name
		       | _ -> ""
		       )
		    )
	      )
          in
          Env.open_signature Asttypes.Override path' (self#extract_sig env mty) env
      in
      Hashtbl.add env_cache (sum, subst) env;
      env
      
  method add_import f =
    if not ( Sm.mem f import )
    then import <- Sm.add f 0 import

end
let r = new restorer

let restore fn s nam =
  let s = r # structure s in
  r # clear_type_table;
  let mtype =
    let cmi = try read_cmi fn with _ -> read_cmi (fn^"i") in
    Types.Mty_signature (cmi.Cmi_format.cmi_sign) in
  r#add_import nam;
  let i = Ident.({stamp = 0; name = nam; flags = 0 }) in
  r#add_module_env i mtype;
  r#reset_cache;
  ( nam, s)
    
let add_interface fn =
  let cmi = read_cmi fn in
  let open Cmi_format in
  r#add_module_env
    Ident.({stamp = 0; name = cmi.cmi_name; flags = 0 })
    ( Types.Mty_signature cmi.cmi_sign)
  
let load_and_restore fn =
  let cmt = read_cmt fn in
  restore fn 
    (match cmt.cmt_annots with
    | Implementation s -> s
    | _ -> assert false )
    cmt.cmt_modname
