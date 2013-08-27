open Typedtree

class mapper :
  object
    method clear_type_table : unit
   (*
     This method should be called between any iteration on a typedtree.
     Otherwise there's no way to certify that the right tree will be returned.
   *)

    (*
      All the methods below are mappers into typedtrees.
      Most of the type declarations may be found in the file typedtree.mli.
      Methods starting with a t_ are mapping types described in types.mli
      The other called modules can be found in parsing/ and typing/
    *)
    method class_declaration :
      class_declaration -> class_declaration
    method class_description :
      class_description -> class_description
    method class_expr : class_expr -> class_expr
    method class_expr_desc :
      class_expr_desc -> class_expr_desc
    method class_field : class_field -> class_field
    method class_field_desc :
      class_field_desc -> class_field_desc
    method class_field_kind :
      class_field_kind -> class_field_kind
    method class_signature :
      class_signature -> class_signature
    method class_structure :
      class_structure -> class_structure
    method class_type : class_type -> class_type
    method class_type_declaration :
      class_type_declaration -> class_type_declaration
    method class_type_desc :
      class_type_desc -> class_type_desc
    method class_type_field :
      class_type_field -> class_type_field
    method class_type_field_desc :
      class_type_field_desc -> class_type_field_desc
    method constructor_description :
      Types.constructor_description -> Types.constructor_description
    method core_field_desc :
      core_field_desc -> core_field_desc
    method core_field_type :
      core_field_type -> core_field_type
    method core_type : core_type -> core_type
    method ctypdesc : core_type_desc -> core_type_desc
    method env : Env.t -> Env.t
    method exception_declaration :
      exception_declaration -> exception_declaration
    method exp_extra : exp_extra -> exp_extra
    method expression : expression -> expression
    method expression_desc :
      expression_desc -> expression_desc
    method ident : Ident.t -> Ident.t
    method label_description :
      Types.label_description -> Types.label_description
    method location : Location.t -> Location.t
    method longident : Longident.t -> Longident.t
    method longident_loc :
      Longident.t Asttypes.loc -> Longident.t Asttypes.loc
    method meth : meth -> meth
    method modtype_declaration :
      modtype_declaration -> modtype_declaration
    method module_coercion :
      module_coercion -> module_coercion
    method module_expr : module_expr -> module_expr
    method module_expr_desc :
      module_expr_desc -> module_expr_desc
    method module_type : module_type -> module_type
    method module_type_constraint :
      module_type_constraint -> module_type_constraint
    method module_type_desc :
      module_type_desc -> module_type_desc
    method package_type : package_type -> package_type
    method pat_extra : pat_extra -> pat_extra
    method path : Path.t -> Path.t
    method pattern : pattern -> pattern
    method pattern_desc : pattern_desc -> pattern_desc
    method row_field : row_field -> row_field
    method signature : signature -> signature
    method signature_item :
      signature_item -> signature_item
    method signature_item_desc :
      signature_item_desc -> signature_item_desc
    method string_loc : string Asttypes.loc -> string Asttypes.loc
    method structure : structure -> structure
    method structure_item :
      structure_item -> structure_item
    method structure_item_desc :
      structure_item_desc -> structure_item_desc
    method t_abbrev_memo : Types.abbrev_memo -> Types.abbrev_memo
    method t_class_declaration :
      Types.class_declaration -> Types.class_declaration
    method t_class_signature : Types.class_signature -> Types.class_signature
    method t_class_type : Types.class_type -> Types.class_type
    method t_class_type_declaration :
      Types.class_type_declaration -> Types.class_type_declaration
    method t_exception_declaration :
      Types.exception_declaration -> Types.exception_declaration
    method t_modtype_declaration :
      Types.modtype_declaration -> Types.modtype_declaration
    method t_module_type : Types.module_type -> Types.module_type
    method t_row_desc : Types.row_desc -> Types.row_desc
    method t_row_field : Types.row_field -> Types.row_field
    method t_signature : Types.signature -> Types.signature
    method t_signature_item : Types.signature_item -> Types.signature_item
    method t_type_declaration :
      Types.type_declaration -> Types.type_declaration
    method t_type_desc : Types.type_desc -> Types.type_desc
    method t_type_expr : Types.type_expr -> Types.type_expr
    method t_type_kind : Types.type_kind -> Types.type_kind
    method t_value_description :
      Types.value_description -> Types.value_description
    method t_value_kind : Types.value_kind -> Types.value_kind
    method type_declaration :
      type_declaration -> type_declaration
    method type_kind : type_kind -> type_kind
    method value_description :
      value_description -> value_description
    method with_constraint :
      with_constraint -> with_constraint
  end
