open Typedtree
open Types

let map_ref f r = ref (f !r)
let map_option f = function Some x -> Some (f x) | None -> None
let map_meths f = Meths.mapi (fun _ x -> f x)
let map f l = List.rev (List.rev_map f l)
let map_tuple f g (a,b) = (f a, g b)
let map2 f g = map (map_tuple f g)
let map3 f g h = map (fun (a,b,c)->(f a, g b, h c))
let map4 f g h i = map (fun (a,b,c,d)->(f a, g b, h c,i d))
let map5 f g h i j = map (fun (a,b,c,d,e)->(f a, g b, h c,i d,j e))
let id x = x

class mapper =
object (self)

  val te_table = Hashtbl.create 100

  method clear_type_table = Hashtbl.reset te_table

  method pattern pat =
    let pat_desc = self#pattern_desc pat.pat_desc in
    let pat_loc = self#location pat.pat_loc in
    let pat_extra = map2 self#pat_extra self#location pat.pat_extra in
    let pat_type = self#t_type_expr pat.pat_type in
    let pat_env = self#env pat.pat_env in
    {
      pat_desc;
      pat_loc;
      pat_extra;
      pat_type;
      pat_env;
    }

  method pat_extra =
    function
    | Tpat_constraint (ctyp ) -> Tpat_constraint (self#core_type ctyp)
    | Tpat_type ( (p, loc) ) ->
      Tpat_type ( self#path p, self#longident_loc loc)
    | Tpat_unpack -> Tpat_unpack

  method pattern_desc =
    function
    | Tpat_any -> Tpat_any
    | Tpat_var ( i, sloc ) ->
      Tpat_var ( self#ident i, self#string_loc sloc)
    | Tpat_alias ( pat, i, sloc ) ->
      Tpat_alias ( self#pattern pat, self#ident i, self#string_loc sloc)
    | Tpat_constant ( constant ) -> Tpat_constant ( constant)
    | Tpat_tuple ( l ) -> Tpat_tuple (map self#pattern l)
    | Tpat_construct ( loc, cdescr, l, b ) ->
      Tpat_construct ( self#longident_loc loc,
		       self#constructor_description cdescr,
		       map self#pattern l,
		       b)
    | Tpat_variant ( label, pat, rdesc ) ->
      Tpat_variant ( label, map_option self#pattern pat, map_ref self#t_row_desc rdesc)
    | Tpat_record ( l, flag ) ->
      Tpat_record (map3 self#longident_loc self#label_description self#pattern l, flag)
    | Tpat_array ( l ) -> Tpat_array (map self#pattern l)
    | Tpat_or ( pat, pat2, row_desc ) ->
      Tpat_or ( self#pattern pat, self#pattern pat2, map_option self#t_row_desc row_desc)
    | Tpat_lazy ( pat ) -> Tpat_lazy ( self#pattern pat )

  method expression exp =
    let exp_desc = self#expression_desc exp.exp_desc in
    let exp_loc = self#location exp.exp_loc in
    let exp_extra = map2 self#exp_extra self#location exp.exp_extra in
    let exp_type = self#t_type_expr exp.exp_type in
    let exp_env = self#env exp.exp_env in
    {
      exp_desc;
      exp_loc;
      exp_extra;
      exp_type;
      exp_env;
    }

  method exp_extra =
    function
    | Texp_constraint ( ctyp, ctyp2 ) ->
      Texp_constraint ( map_option self#core_type ctyp, map_option self#core_type ctyp2 )
    | Texp_open ( flag, path, loc, env ) ->
      Texp_open ( flag, self#path path, self#longident_loc loc, self#env env )
    | Texp_poly ( ctyp ) ->
      Texp_poly ( map_option self#core_type ctyp )
    | Texp_newtype ( s ) -> Texp_newtype ( s )

  method expression_desc =
    function
    | Texp_ident ( p, loc, vd ) ->
      Texp_ident ( self#path p, self#longident_loc loc, self#t_value_description vd)
    | Texp_constant ( c ) -> Texp_constant ( c )
    | Texp_let ( flag, l, expr ) ->
      Texp_let ( flag, map2 self#pattern self#expression l, self#expression expr )
    | Texp_function ( label, l, partial ) ->
      Texp_function ( label, map2 self#pattern self#expression l, partial )
    | Texp_apply ( expr, l ) ->
      Texp_apply ( self#expression expr, map3 id (map_option self#expression) id l )
    | Texp_match ( expr, l, partial ) ->
      Texp_match ( self#expression expr, map2 self#pattern self#expression l, partial )
    | Texp_try ( expr, l ) ->
      Texp_try ( self#expression expr, map2 self#pattern self#expression l )
    | Texp_tuple ( l ) -> Texp_tuple (map self#expression l )
    | Texp_construct ( loc, cdescr, l, b ) ->
      Texp_construct ( self#longident_loc loc,
		       self#constructor_description cdescr,
		       map self#expression l, b )
    | Texp_variant ( label, expr ) ->
      Texp_variant ( label, map_option self#expression expr )
    | Texp_record ( l, expr ) ->
      Texp_record ( map3 self#longident_loc self#label_description self#expression l,
		    map_option self#expression expr )
    | Texp_field ( expr, loc, ldescr ) ->
      Texp_field ( self#expression expr, self#longident_loc loc, self#label_description ldescr)
    | Texp_setfield ( expr, loc, ldescr, expr2 ) ->
      Texp_setfield ( self#expression expr, self#longident_loc loc, self#label_description ldescr, self#expression expr2)
    | Texp_array ( l ) ->
      Texp_array (map self#expression l)
    | Texp_ifthenelse ( expr, expr2, expr3 ) ->
      Texp_ifthenelse ( self#expression expr, self#expression expr2, map_option self#expression expr3)
    | Texp_sequence ( expr, expr2 ) ->
      Texp_sequence ( self#expression expr, self#expression expr2)
    | Texp_while ( expr, expr2 ) ->
      Texp_while ( self#expression expr, self#expression expr2)
    | Texp_for ( i, sloc, expr, expr2, flag, expr3 ) ->
      Texp_for ( self#ident i, self#string_loc sloc, self#expression expr, self#expression expr2, flag, self#expression expr3)
    | Texp_when ( expr, expr2 ) ->
      Texp_when ( self#expression expr, self#expression expr2)
    | Texp_send ( expr, meth, expr2 ) ->
      Texp_send ( self#expression expr, self#meth meth, map_option self#expression expr2)
    | Texp_new ( p, loc, cdecl ) ->
      Texp_new ( self#path p, self#longident_loc loc, self#t_class_declaration cdecl)
    | Texp_instvar ( p, p2, sloc ) ->
      Texp_instvar ( self#path p, self#path p2, self#string_loc sloc)
    | Texp_setinstvar ( p, p2, sloc, expr ) ->
      Texp_setinstvar ( self#path p, self#path p2, self#string_loc sloc, self#expression expr)
    | Texp_override ( p, l ) ->
      Texp_override ( self#path p, map3 self#path self#string_loc self#expression l)
    | Texp_letmodule ( i, sloc, mexpr, expr ) ->
      Texp_letmodule ( self#ident i, self#string_loc sloc, self#module_expr mexpr, self#expression expr)
    | Texp_assert ( expr ) -> Texp_assert ( self#expression expr )
    | Texp_assertfalse -> Texp_assertfalse
    | Texp_lazy ( expr ) -> Texp_lazy ( self#expression expr )
    | Texp_object ( cstr, l ) ->
      Texp_object ( self#class_structure cstr, l)
    | Texp_pack (mexpr ) -> Texp_pack (self#module_expr mexpr)

  method meth =
    function
    | Tmeth_name ( s ) -> Tmeth_name ( s )
    | Tmeth_val ( i ) -> Tmeth_val (self#ident i)


  method class_expr cl =
    let cl_desc = self#class_expr_desc cl.cl_desc in
    let cl_loc = self#location cl.cl_loc in
    let cl_type = self#t_class_type cl.cl_type in
    let cl_env = self#env cl.cl_env in
    {
      cl_desc;
      cl_loc;
      cl_type;
      cl_env;
    }

  method class_expr_desc =
    function
    | Tcl_ident (p, loc, l ) ->
      Tcl_ident ( self#path p, self#longident_loc loc, map self#core_type l)
    | Tcl_structure (cstr ) -> Tcl_structure (self#class_structure cstr)
    | Tcl_fun ( label, pat, l, cexpr, partial ) ->
      Tcl_fun ( label, self#pattern pat, map3 self#ident self#string_loc self#expression l,
		self#class_expr cexpr, partial )
    | Tcl_apply ( cexpr, l ) ->
      Tcl_apply ( self#class_expr cexpr, map3 id (map_option self#expression) id l)
    | Tcl_let ( flag, l1, l2, cexpr ) ->
      Tcl_let ( flag,
		map2 self#pattern self#expression l1,
		map3 self#ident self#string_loc self#expression l2,
		self#class_expr cexpr )
    | Tcl_constraint ( cexpr, ctyp, l1, l2, concr ) ->
      Tcl_constraint ( self#class_expr cexpr, map_option self#class_type ctyp, l1, l2, concr )

  method class_structure cstr =
    let cstr_pat = self#pattern cstr.cstr_pat in
    let cstr_fields = map self#class_field cstr.cstr_fields in
    let cstr_type = self#t_class_signature cstr.cstr_type in
    let cstr_meths = map_meths self#ident cstr.cstr_meths in
    {
      cstr_pat;
      cstr_fields;
      cstr_type;
      cstr_meths;
    }

  method class_field cf =
    let cf_desc = self#class_field_desc cf.cf_desc in
    let cf_loc = self#location cf.cf_loc in
    {
      cf_desc;
      cf_loc;
    }

  method class_field_kind =
    function
    | Tcfk_virtual ( ctyp ) -> Tcfk_virtual ( self#core_type ctyp )
    | Tcfk_concrete ( expr ) -> Tcfk_concrete ( self#expression expr )

  method class_field_desc =
    function
    | Tcf_inher ( flag, cexpr, s, l1, l2 ) ->
      Tcf_inher ( flag, self#class_expr cexpr, s, map2 id self#ident l1, map2 id self#ident l2)
    | Tcf_val ( s, sloc, flag_mut, i, cf_kind, b ) ->
      Tcf_val ( s, self#string_loc sloc, flag_mut, self#ident i, self#class_field_kind cf_kind, b)
    | Tcf_meth ( s, sloc, flag_priv, cf_kind, b ) ->
      Tcf_meth ( s, self#string_loc sloc, flag_priv, self#class_field_kind cf_kind, b)
    | Tcf_constr ( ctyp, ctyp2 ) ->
      Tcf_constr ( self#core_type ctyp, self#core_type ctyp2 )
    | Tcf_init ( expr ) -> Tcf_init ( self#expression expr )


  method module_expr mod_e =
    let mod_desc = self#module_expr_desc mod_e.mod_desc in
    let mod_loc = self#location mod_e.mod_loc in
    let mod_type = self#t_module_type mod_e.mod_type in
    let mod_env = self#env mod_e.mod_env in
    {
      mod_desc;
      mod_loc;
      mod_type;
      mod_env;
    }

  method module_type_constraint =
    function
    | Tmodtype_implicit -> Tmodtype_implicit
    | Tmodtype_explicit ( mtyp ) -> Tmodtype_explicit ( self#module_type mtyp )

  method module_expr_desc =
    function
    | Tmod_ident ( p, loc ) ->
      Tmod_ident ( self#path p, self#longident_loc loc )
    | Tmod_structure ( str ) -> Tmod_structure ( self#structure str )
    | Tmod_functor ( i, sloc, mtyp, mexpr ) ->
      Tmod_functor ( self#ident i, self#string_loc sloc, self#module_type mtyp, self#module_expr mexpr)
    | Tmod_apply ( mexpr, mexpr2, mcoerce ) ->
      Tmod_apply ( self#module_expr mexpr, self#module_expr mexpr2, self#module_coercion mcoerce )
    | Tmod_constraint ( mexpr, mtyp, mconstr, mcoerce ) ->
      Tmod_constraint ( self#module_expr mexpr, self#t_module_type mtyp, self#module_type_constraint mconstr, self#module_coercion mcoerce)
    | Tmod_unpack ( expr, mtyp ) ->
      Tmod_unpack ( self#expression expr, self#t_module_type mtyp)

  method structure str =
    let str_items = map self#structure_item str.str_items in
    let str_type = self#t_signature str.str_type in
    let str_final_env = self#env str.str_final_env in
    {
      str_items;
      str_type;
      str_final_env;
    }

  method structure_item str =
    let str_desc = self#structure_item_desc str.str_desc in
    let str_loc = self#location str.str_loc in
    let str_env = self#env str.str_env in
    {
      str_desc;
      str_loc;
      str_env;
    }

  method structure_item_desc =
    function
    | Tstr_eval (expr ) -> Tstr_eval ( self#expression expr )
    | Tstr_value ( flag, l ) ->
      Tstr_value ( flag, map2 self#pattern self#expression l )
    | Tstr_primitive ( i, sloc, vdescr ) ->
      Tstr_primitive ( self#ident i, self#string_loc sloc, self#value_description vdescr )
    | Tstr_type ( l ) ->
      Tstr_type ( map3 self#ident self#string_loc self#type_declaration l )
    | Tstr_exception ( i, sloc, exn_decl ) ->
      Tstr_exception ( self#ident i, self#string_loc sloc, self#exception_declaration exn_decl )
    | Tstr_exn_rebind ( i, sloc, p, loc ) ->
      Tstr_exn_rebind ( self#ident i, self#string_loc sloc, self#path p, self#longident_loc loc )
    | Tstr_module ( i, sloc, mexpr ) ->
      Tstr_module ( self#ident i, self#string_loc sloc, self#module_expr mexpr )
    | Tstr_recmodule ( l ) ->
      Tstr_recmodule (map4 self#ident self#string_loc self#module_type self#module_expr l )
    | Tstr_modtype ( i, sloc, mtyp ) ->
      Tstr_modtype ( self#ident i, self#string_loc sloc, self#module_type mtyp )
    | Tstr_open ( flag, path, loc) ->
      Tstr_open ( flag, self#path path, self#longident_loc loc )
    | Tstr_class ( l ) ->
      Tstr_class ( map3 self#class_declaration id id l )
    | Tstr_class_type ( l ) ->
      Tstr_class_type ( map3 self#ident self#string_loc self#class_type_declaration l )
    | Tstr_include ( mexpr, l ) ->
      Tstr_include ( self#module_expr mexpr, map self#ident l)

  method module_coercion =
    function
    | Tcoerce_none -> Tcoerce_none
    | Tcoerce_structure ( l ) ->
      Tcoerce_structure ( map2 id self#module_coercion l )
    | Tcoerce_functor ( mcoerce, mcoerce2 ) ->
      Tcoerce_functor ( self#module_coercion mcoerce, self#module_coercion mcoerce2)
    | Tcoerce_primitive ( prim ) -> Tcoerce_primitive ( prim )

  method module_type mty =
    let mty_desc = self#module_type_desc mty.mty_desc in
    let mty_type = self#t_module_type mty.mty_type in
    let mty_env = self#env mty.mty_env in
    let mty_loc = self#location mty.mty_loc in
    {
      mty_desc;
      mty_type;
      mty_env;
      mty_loc;
    }

  method module_type_desc =
    function
    | Tmty_ident ( p, loc ) ->
      Tmty_ident ( self#path p, self#longident_loc loc )
    | Tmty_signature ( sign ) -> Tmty_signature ( self#signature sign )
    | Tmty_functor ( i, sloc, mtyp, mtyp2  ) ->
      Tmty_functor ( self#ident i, self#string_loc sloc, self#module_type mtyp,self#module_type mtyp2 )
    | Tmty_with ( mtyp, l ) ->
      Tmty_with ( self#module_type mtyp, map3 self#path self#longident_loc self#with_constraint l )
    | Tmty_typeof ( mexpr ) -> Tmty_typeof ( self#module_expr mexpr )

  method signature sign =
    let sig_items = map self#signature_item sign.sig_items in
    let sig_type = self#t_signature sign.sig_type in
    let sig_final_env = self#env sign.sig_final_env in
    {
      sig_items;
      sig_type;
      sig_final_env;
    }

  method signature_item sign =
    let sig_desc = self#signature_item_desc sign.sig_desc in
    let sig_env = self#env sign.sig_env in
    let sig_loc = self#location sign.sig_loc in
    {
      sig_desc;
      sig_env;
      sig_loc;
    }

  method signature_item_desc =
    function
    | Tsig_value ( i, sloc, vdescr ) ->
      Tsig_value ( self#ident i, self#string_loc sloc, self#value_description vdescr )
    | Tsig_type ( l ) ->
      Tsig_type ( map3 self#ident self#string_loc self#type_declaration l )
    | Tsig_exception ( i, sloc, exn_decl ) ->
      Tsig_exception ( self#ident i, self#string_loc sloc, self#exception_declaration exn_decl )
    | Tsig_module ( i, sloc, mtyp ) ->
      Tsig_module ( self#ident i, self#string_loc sloc, self#module_type mtyp )
    | Tsig_recmodule ( l ) ->
      Tsig_recmodule ( map3 self#ident self#string_loc self#module_type l )
    | Tsig_modtype ( i, sloc, mtyp_decl ) ->
      Tsig_modtype ( self#ident i, self#string_loc sloc, self#modtype_declaration mtyp_decl )
    | Tsig_open ( flag, path, loc ) ->
      Tsig_open (  flag, self#path path, self#longident_loc loc )
    | Tsig_include ( mtyp, sign ) ->
      Tsig_include ( self#module_type mtyp, self#t_signature sign )
    | Tsig_class ( l ) ->
      Tsig_class ( map self#class_description l )
    | Tsig_class_type ( l ) ->
      Tsig_class_type ( map self#class_type_declaration l )

  method modtype_declaration =
    function
    | Tmodtype_abstract -> Tmodtype_abstract
    | Tmodtype_manifest ( mtyp ) -> Tmodtype_manifest (self#module_type mtyp )

  method with_constraint =
    function
    | Twith_type ( typ_decl ) -> Twith_type ( self#type_declaration typ_decl )
    | Twith_module ( p, loc ) ->
      Twith_module ( self#path p, self#longident_loc loc )
    | Twith_typesubst ( typ_decl ) -> Twith_typesubst ( self#type_declaration typ_decl )
    | Twith_modsubst ( p, loc ) ->
      Twith_modsubst ( self#path p, self#longident_loc loc )

  method core_type ctyp =
    let ctyp_desc = self#ctypdesc ctyp.ctyp_desc in
    let ctyp_type = self#t_type_expr ctyp.ctyp_type in
    let ctyp_env = self#env ctyp.ctyp_env in
    let ctyp_loc = self#location ctyp.ctyp_loc in
    {
      ctyp_desc;
      ctyp_type;
      ctyp_env;
      ctyp_loc;
    }

  method ctypdesc =
    function
    | Ttyp_any -> Ttyp_any
    | Ttyp_var (s ) -> Ttyp_var ( s )
    | Ttyp_arrow ( label, ctyp, ctyp2 ) ->
      Ttyp_arrow ( label, self#core_type ctyp, self#core_type ctyp2)
    | Ttyp_tuple ( l ) ->
      Ttyp_tuple (map self#core_type l)
    | Ttyp_constr ( p, loc, l ) ->
      Ttyp_constr ( self#path p, self#longident_loc loc, map self#core_type l)
    | Ttyp_object ( l ) ->
      Ttyp_object (map self#core_field_type l)
    | Ttyp_class ( p, loc, l1, l2 ) ->
      Ttyp_class ( self#path p, self#longident_loc loc, map self#core_type l1, l2 )
    | Ttyp_alias ( ctyp, s ) ->
      Ttyp_alias ( self#core_type ctyp, s )
    | Ttyp_variant ( l1, b, l2 ) ->
      Ttyp_variant ( map self#row_field l1, b, l2)
    | Ttyp_poly ( l, ctyp ) ->
      Ttyp_poly ( l, self#core_type ctyp )
    | Ttyp_package ( pack ) -> Ttyp_package ( self#package_type pack )

  method package_type pack =
    let pack_name = self#path pack.pack_name in
    let pack_fields = map2 self#longident_loc self#core_type pack.pack_fields in
    let pack_type = self#t_module_type pack.pack_type in
    let pack_txt = self#longident_loc pack.pack_txt in
    {
      pack_name;
      pack_fields;
      pack_type;
      pack_txt;
    }

  method core_field_type field =
    let field_desc = self#core_field_desc field.field_desc in
    let field_loc = self#location field.field_loc in
    {
      field_desc;
      field_loc;
    }

  method core_field_desc =
    function
    | Tcfield ( s, ctyp ) ->
      Tcfield ( s, self#core_type ctyp )
    | Tcfield_var -> Tcfield_var

  method row_field =
    function
    | Ttag ( label, b, l ) ->
      Ttag ( label, b, map self#core_type l )
    | Tinherit ( ctyp ) -> Tinherit ( self#core_type ctyp )

  method value_description val_d =
    let val_desc = self#core_type val_d.val_desc in
    let val_val = self#t_value_description val_d.val_val in
    let val_prim = map id val_d.val_prim in
    let val_loc = self#location val_d.val_loc in
    {
      val_desc;
      val_val;
      val_prim;
      val_loc;
    }

  method type_declaration typ =
    let typ_type = self#t_type_declaration typ.typ_type in
    let typ_cstrs = map3 self#core_type self#core_type self#location typ.typ_cstrs in
    let typ_kind = self#type_kind typ.typ_kind in
    let typ_private = typ.typ_private in
    let typ_manifest = map_option self#core_type typ.typ_manifest in
    let typ_loc = self#location typ.typ_loc in
    { typ with
      typ_type;
      typ_cstrs;
      typ_kind;
      typ_private;
      typ_manifest;
      typ_loc;
    }

  method type_kind =
    function
    | Ttype_abstract -> Ttype_abstract
    | Ttype_variant ( l ) ->
      Ttype_variant ( map4 self#ident self#string_loc (map self#core_type) self#location l)
    | Ttype_record ( l ) ->
      Ttype_record ( map5 self#ident self#string_loc id self#core_type self#location l )

  method exception_declaration exn_d =
    let exn_params = map self#core_type exn_d.exn_params in
    let exn_exn = self#t_exception_declaration exn_d.exn_exn in
    let exn_loc = self#location exn_d.exn_loc in
    {
      exn_params;
      exn_exn;
      exn_loc;
    }

  method class_type cltyp =
    let cltyp_desc = self#class_type_desc cltyp.cltyp_desc in
    let cltyp_type = self#t_class_type cltyp.cltyp_type in
    let cltyp_env = self#env cltyp.cltyp_env in
    let cltyp_loc = self#location cltyp.cltyp_loc in
    {
      cltyp_desc;
      cltyp_type;
      cltyp_env;
      cltyp_loc;
    }

  method class_type_desc =
    function
    | Tcty_constr ( p, loc, l ) ->
      Tcty_constr ( self#path p, self#longident_loc loc, map self#core_type l)
    | Tcty_signature ( csign ) -> Tcty_signature (self#class_signature csign )
    | Tcty_fun ( label, ctyp, cltyp ) ->
      Tcty_fun ( label, self#core_type ctyp, self#class_type cltyp)

  method class_signature csig =
    let csig_self = self#core_type csig.csig_self in
    let csig_fields = map self#class_type_field csig.csig_fields in
    let csig_type = self#t_class_signature csig.csig_type in
    let csig_loc = self#location csig.csig_loc in
    {
      csig_self;
      csig_fields;
      csig_type;
      csig_loc;
    }

  method class_type_field ctf =
    let ctf_desc = self#class_type_field_desc ctf.ctf_desc in
    let ctf_loc = self#location ctf.ctf_loc in
    {
      ctf_desc;
      ctf_loc;
    }

  method class_type_field_desc =
    function
    | Tctf_inher (cltyp ) -> Tctf_inher (self#class_type cltyp)
    | Tctf_val ( s, flag_mut, flag_virt, ctyp ) ->
      Tctf_val (  s, flag_mut, flag_virt, self#core_type ctyp )
    | Tctf_virt ( s, flag_priv, ctyp) ->
      Tctf_virt ( s, flag_priv, self#core_type ctyp )
    | Tctf_meth ( s, flag_priv, ctyp ) ->
      Tctf_meth ( s, flag_priv, self#core_type ctyp)
    | Tctf_cstr ( ctyp, ctyp2 ) ->
      Tctf_cstr ( self#core_type ctyp, self#core_type ctyp2 )

  method class_declaration cdecl =
    let ci_params = (let (a,b) = cdecl.ci_params in ((map self#string_loc a), self#location b)) in
    let ci_id_name = self#string_loc cdecl.ci_id_name in
    let ci_id_class = self#ident cdecl.ci_id_class in
    let ci_id_class_type = self#ident cdecl.ci_id_class_type in
    let ci_id_object = self#ident cdecl.ci_id_object in
    let ci_id_typesharp = self#ident cdecl.ci_id_typesharp in
    let ci_expr = self#class_expr cdecl.ci_expr in
    let ci_decl = self#t_class_declaration cdecl.ci_decl in
    let ci_type_decl = self#t_class_type_declaration cdecl.ci_type_decl in
    let ci_loc = self#location cdecl.ci_loc in
    { cdecl with
      ci_params;
      ci_id_name;
      ci_id_class;
      ci_id_class_type;
      ci_id_object;
      ci_id_typesharp;
      ci_expr;
      ci_decl;
      ci_type_decl;
      ci_loc;
    }

  method class_description cdescr =
    let ci_params = (let (a,b) = cdescr.ci_params in ((map self#string_loc a), self#location b)) in
    let ci_id_name = self#string_loc cdescr.ci_id_name in
    let ci_id_class = self#ident cdescr.ci_id_class in
    let ci_id_class_type = self#ident cdescr.ci_id_class_type in
    let ci_id_object = self#ident cdescr.ci_id_object in
    let ci_id_typesharp = self#ident cdescr.ci_id_typesharp in
    let ci_expr = self#class_type cdescr.ci_expr in
    let ci_decl = self#t_class_declaration cdescr.ci_decl in
    let ci_type_decl = self#t_class_type_declaration cdescr.ci_type_decl in
    let ci_loc = self#location cdescr.ci_loc in
    { cdescr with
      ci_params;
      ci_id_name;
      ci_id_class;
      ci_id_class_type;
      ci_id_object;
      ci_id_typesharp;
      ci_expr;
      ci_decl;
      ci_type_decl;
      ci_loc;
    }

  method class_type_declaration ctdecl =
    let ci_params = (let (a,b) = ctdecl.ci_params in ((map self#string_loc a), self#location b)) in
    let ci_id_name = self#string_loc ctdecl.ci_id_name in
    let ci_id_class = self#ident ctdecl.ci_id_class in
    let ci_id_class_type = self#ident ctdecl.ci_id_class_type in
    let ci_id_object = self#ident ctdecl.ci_id_object in
    let ci_id_typesharp = self#ident ctdecl.ci_id_typesharp in
    let ci_expr = self#class_type ctdecl.ci_expr in
    let ci_decl = self#t_class_declaration ctdecl.ci_decl in
    let ci_type_decl = self#t_class_type_declaration ctdecl.ci_type_decl in
    let ci_loc = self#location ctdecl.ci_loc in
    { ctdecl with
      ci_params;
      ci_id_name;
      ci_id_class;
      ci_id_class_type;
      ci_id_object;
      ci_id_typesharp;
      ci_expr;
      ci_decl;
      ci_type_decl;
      ci_loc;
    }


  (* mapping of Types start with t_ to avoid collision *)
  method t_value_description vd =
    let val_type = self#t_type_expr vd.val_type in
    let val_kind = self#t_value_kind vd.val_kind in
    let val_loc = self#location vd.val_loc in
    {
      val_type;
      val_kind;
      val_loc;
    }
  method t_type_declaration td =
    let type_params = map self#t_type_expr td.type_params in
    let type_kind = self#t_type_kind td.type_kind in
    let type_manifest = map_option self#t_type_expr td.type_manifest in
    let type_loc = self#location td.type_loc in
    { td with
      type_params;
      type_kind;
      type_manifest;
      type_loc;
    }
  method t_signature = map self#t_signature_item
  method t_signature_item = function
  | Sig_value ( i , vd ) -> Sig_value ( self#ident i , self#t_value_description vd )
  | Sig_type ( i , td , r ) ->Sig_type ( self#ident i , self#t_type_declaration td , r)
  | Sig_exception ( i , exnd ) -> Sig_exception ( self#ident i , self#t_exception_declaration exnd )
  | Sig_module ( i , mt , r ) -> Sig_module ( self#ident i , self#t_module_type mt, r )
  | Sig_modtype ( i , md ) -> Sig_modtype ( self#ident i , self#t_modtype_declaration md )
  | Sig_class ( i , cd , r ) -> Sig_class ( self#ident i , self#t_class_declaration cd , r )
  | Sig_class_type ( i , ctd , r ) -> Sig_class_type ( self#ident i , self#t_class_type_declaration ctd, r )

  method t_module_type = function
  | Mty_ident p -> Mty_ident ( self#path p )
  | Mty_signature s -> Mty_signature ( self#t_signature s )
  | Mty_functor ( i , mtyp , mtyp2) -> Mty_functor ( self#ident i, self#t_module_type mtyp, self#t_module_type mtyp2)

  method t_exception_declaration ed =
    let exn_args = map self#t_type_expr ed.exn_args in
    let exn_loc = self#location ed.exn_loc in
    {
      exn_args;
      exn_loc;
    }

  method t_class_type_declaration ctd =
    let clty_params = map self#t_type_expr ctd.clty_params in
    let clty_type = self#t_class_type ctd.clty_type in
    let clty_path = self#path ctd.clty_path in
    { ctd with
      clty_params;
      clty_type;
      clty_path;
    }

  method t_class_type = function
  | Cty_constr ( p ,l ,ct ) -> Cty_constr ( self#path p, map self#t_type_expr l, self#t_class_type ct)
  | Cty_signature ( s ) -> Cty_signature ( self#t_class_signature s )
  | Cty_fun ( label , typ , ctyp ) -> Cty_fun ( label , self#t_type_expr typ , self#t_class_type ctyp )

  method t_class_signature sgn =
    let cty_self = self#t_type_expr sgn.cty_self in
    let cty_inher = map2 self#path (map self#t_type_expr) sgn.cty_inher in
	{ sgn with
	  cty_self;
	  cty_inher;
	}
  method t_class_declaration cd =
    let cty_params = map self#t_type_expr cd.cty_params in
    let cty_type = self#t_class_type cd.cty_type in
    let cty_path = self#path cd.cty_path in
    let cty_new = map_option self#t_type_expr cd.cty_new in
    { cd with
      cty_params;
      cty_type;
      cty_path;
      cty_new;
    }

  method t_value_kind = function
  | Val_self ( meths, vars, s, expr ) ->
    Val_self ( map_ref ( map_meths ( map_tuple self#ident  self#t_type_expr )) meths,
	       map_ref ( Vars.mapi (fun s (i, mut, virt, e) -> (self#ident i, mut, virt, self#t_type_expr e))) vars,
	       s, self#t_type_expr expr)
  | Val_anc ( l, s ) -> Val_anc ( map2 id self#ident l, s)
  | v -> v

  method t_type_kind = function
  | Type_abstract -> Type_abstract
  | Type_record ( l, repr ) -> Type_record ( map3 self#ident id self#t_type_expr l, repr )
  | Type_variant l -> Type_variant ( map3 self#ident ( map self#t_type_expr ) ( map_option self#t_type_expr ) l )

  method t_type_expr expr =
    if Hashtbl.mem te_table expr.id
    then Hashtbl.find te_table expr.id
    else
      begin
	let expr2 = { expr with desc = Tnil } in
	Hashtbl.add te_table expr.id expr2;
	let d = self#t_type_desc expr.desc in
	expr2.desc <- d;
	expr2
      end
	
  method t_type_desc = function
  | Tarrow ( label, e1, e2, c ) -> Tarrow ( label , self#t_type_expr e1 , self#t_type_expr e2 , c)
  | Ttuple ( l ) -> Ttuple ( map self#t_type_expr l )
  | Tconstr ( p, l, memo ) -> Tconstr ( self#path p , map self#t_type_expr l, map_ref self#t_abbrev_memo memo )
  | Tobject ( e, o ) -> Tobject ( self#t_type_expr e , map_ref (map_option (map_tuple self#path (map self#t_type_expr))) o )
  | Tfield ( s, k, e1, e2 ) -> Tfield ( s , k , self#t_type_expr e1 , self#t_type_expr e2 )
  | Tlink ( e ) -> Tlink ( self#t_type_expr e )
  | Tsubst ( e ) -> Tsubst ( self#t_type_expr e )
  | Tvariant ( r ) -> Tvariant ( self#t_row_desc r )
  | Tpoly ( e,l ) -> Tpoly ( self#t_type_expr e, map self#t_type_expr l )
  | Tpackage ( p, l, exprs ) -> Tpackage ( self#path p , map self#longident l , map self#t_type_expr exprs )
  | d -> d

  method t_modtype_declaration = function
  | Modtype_abstract -> Modtype_abstract
  | Modtype_manifest ( mt ) -> Modtype_manifest ( self#t_module_type mt )

  method t_row_desc row =
    let row_fields = map2 id self#t_row_field row.row_fields in
    let row_more = self#t_type_expr row.row_more in
    let row_name = map_option (map_tuple self#path  (map self#t_type_expr)) row.row_name in
    { row with
      row_fields;
      row_more;
      row_name;
    }

  method t_row_field = function
  | Rpresent ( e ) -> Rpresent ( map_option self#t_type_expr e )
  | Reither ( b1 ,l , b2 , field ) -> Reither ( b1 , map self#t_type_expr l , b2 , map_ref (map_option self#t_row_field) field )
  | Rabsent -> Rabsent

  method t_abbrev_memo = function
  | Mnil -> Mnil
  | Mcons ( flag, p, e1, e2, m ) -> Mcons (flag, self#path p, self#t_type_expr e1,self#t_type_expr e2, self#t_abbrev_memo m)
  | Mlink ( m ) -> Mlink ( map_ref self#t_abbrev_memo m)

  method longident x = x
  method string_loc x = x

  method path = let open Path in function
  | Pident i -> Pident (self#ident i)
  | Pdot (p,s,i) -> Pdot (self#path p, s, i)
  | Papply (p1,p2) -> Papply (self#path p1, self#path p2)

  method longident_loc x = x
  method location x = x
  method label_description x = x
  method ident x = x
  method env x = x
  method constructor_description x = x

end
