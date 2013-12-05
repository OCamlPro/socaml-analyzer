open Typedtree

let restore = Envaux.env_of_only_summary

module ReEnv  = TypedtreeMap.MakeMap (struct
    open TypedtreeMap
    include DefaultMapArgument

    let leave_pattern p = { p with pat_env = restore p.pat_env }
    let leave_expression e =
      let exp_extra = List.map (function
            (Texp_open (ovf, path, lloc, env), loc) ->
            (Texp_open (ovf, path, lloc, restore env), loc)
          | exp_extra -> exp_extra) e.exp_extra in
      { e with
        exp_env = restore e.exp_env;
        exp_extra = exp_extra }
    let leave_class_expr c =
      { c with cl_env = restore c.cl_env }
    let leave_module_expr m =
      { m with mod_env = restore m.mod_env }
    let leave_structure s =
      { s with str_final_env = restore s.str_final_env }
    let leave_structure_item str =
      { str with str_env = restore str.str_env }
    let leave_module_type m =
      { m with mty_env = restore m.mty_env }
    let leave_signature s =
      { s with sig_final_env = restore s.sig_final_env }
    let leave_signature_item s =
      { s with sig_env = restore s.sig_env }
    let leave_core_type c =
      { c with ctyp_env = restore c.ctyp_env }
    let leave_class_type c =
      { c with cltyp_env = restore c.cltyp_env }

  end)


let restore_annots =
  let open Cmt_format in function
    | Implementation s -> Implementation ( ReEnv.map_structure s )
    | _ -> failwith "Not an implementation cmt file"


let restore_cmt_env cmtf =
  let open Cmt_format in
  if cmtf.cmt_use_summaries
  then
    { cmtf with
      cmt_initial_env = restore cmtf.cmt_initial_env;
      cmt_annots = restore_annots cmtf.cmt_annots;
      cmt_use_summaries = false;
    }
  else cmtf
