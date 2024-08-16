open Ppxlib
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let yojson_of_loc _ _  = yojson_of_string "FIXME"
let yojson_of_longident_loc _ = yojson_of_string "FIXME"

let yojson_of_open_infos _ _ = yojson_of_string "type_unsupported"
let yojson_of_include_infos _ _ = yojson_of_string "type_unsupported"
let yojson_of_class_infos _ _ = yojson_of_string "type_unsupported"

[%%import:
type rec_flag = Ppxlib__.Import.rec_flag [@@deriving  yojson_of]
and arg_label = Ppxlib__.Import.arg_label [@@deriving  yojson_of]
and attribute = Ppxlib__.Import.attribute [@@deriving  yojson_of]
and attributes = Ppxlib__.Import.attributes [@@deriving yojson_of]
and binding_op = Ppxlib__.Import.binding_op [@@deriving yojson_of]
and case = Ppxlib__.Import.case [@@deriving  yojson_of]
and cases = Ppxlib__.Import.cases [@@deriving  yojson_of]
and class_declaration = Ppxlib__.Import.class_declaration [@@deriving yojson_of]
and class_description = Ppxlib__.Import.class_description [@@deriving  yojson_of]
and class_expr = Ppxlib__.Import.class_expr [@@deriving  yojson_of]
and class_expr_desc = Ppxlib__.Import.class_expr_desc [@@deriving yojson_of]
and class_field = Ppxlib__.Import.class_field [@@deriving  yojson_of]
and class_field_desc = Ppxlib__.Import.class_field_desc [@@deriving  yojson_of]
and class_field_kind = Ppxlib__.Import.class_field_kind [@@deriving  yojson_of]
and class_signature  = Ppxlib__.Import.class_signature [@@deriving  yojson_of]
and class_structure = Ppxlib__.Import.class_structure [@@deriving  yojson_of]
and class_type = Ppxlib__.Import.class_type [@@deriving  yojson_of]
and class_type_declaration = Ppxlib__.Import.class_type_declaration [@@deriving  yojson_of]
and class_type_desc = Ppxlib__.Import.class_type_desc [@@deriving  yojson_of]
and class_type_field = Ppxlib__.Import.class_type_field [@@deriving  yojson_of]
and class_type_field_desc = Ppxlib__.Import.class_type_field_desc [@@deriving  yojson_of]
and closed_flag = Ppxlib__.Import.closed_flag [@@deriving  yojson_of]
and constant = Ppxlib__.Import.constant [@@deriving  yojson_of]
and constructor_arguments = Ppxlib__.Import.constructor_arguments [@@deriving  yojson_of]
and constructor_declaration = Ppxlib__.Import.constructor_declaration [@@deriving  yojson_of]
and core_type = Ppxlib__.Import.core_type [@@deriving  yojson_of]
and core_type_desc = Ppxlib__.Import.core_type_desc [@@deriving  yojson_of]
and direction_flag = Ppxlib__.Import.direction_flag [@@deriving yojson_of]
and expression = Ppxlib__.Import.expression [@@deriving yojson_of]
and expression_desc = Ppxlib__.Import.expression_desc [@@deriving yojson_of]
and extension = Ppxlib__.Import.extension [@@deriving  yojson_of]
and extension_constructor = Ppxlib__.Import.extension_constructor [@@deriving  yojson_of]
and extension_constructor_kind = Ppxlib__.Import.extension_constructor_kind [@@deriving  yojson_of]
and functor_parameter = Ppxlib__.Import.functor_parameter [@@deriving  yojson_of]
and include_declaration = Ppxlib__.Import.include_declaration [@@deriving yojson_of]
and include_description = Ppxlib__.Import.include_description [@@deriving  yojson_of]
and injectivity = Ppxlib__.Import.injectivity [@@deriving  yojson_of]
and label = Ppxlib__.Import.label [@@deriving  yojson_of]
and label_declaration = Ppxlib__.Import.label_declaration [@@deriving  yojson_of]
and letop = Ppxlib__.Import.letop [@@deriving yojson_of]
and location = Ppxlib__.Import.location [@@deriving  yojson_of]
and location_stack = Ppxlib__.Import.location_stack [@@deriving  yojson_of]
and longident = Ppxlib__.Import.longident [@@deriving  yojson_of]
and module_binding = Ppxlib__.Import.module_binding [@@deriving yojson_of]
and module_declaration = Ppxlib__.Import.module_declaration [@@deriving  yojson_of]
and module_expr = Ppxlib__.Import.module_expr [@@deriving  yojson_of]
and module_expr_desc = Ppxlib__.Import.module_expr_desc [@@deriving  yojson_of]
and module_substitution = Ppxlib__.Import.module_substitution [@@deriving  yojson_of]
and module_type = Ppxlib__.Import.module_type [@@deriving  yojson_of]
and module_type_declaration = Ppxlib__.Import.module_type_declaration [@@deriving  yojson_of]
and module_type_desc = Ppxlib__.Import.module_type_desc [@@deriving  yojson_of]
and mutable_flag = Ppxlib__.Import.mutable_flag [@@deriving  yojson_of]
and object_field = Ppxlib__.Import.object_field [@@deriving  yojson_of]
and object_field_desc = Ppxlib__.Import.object_field_desc [@@deriving  yojson_of]
and open_declaration = Ppxlib__.Import.open_declaration [@@deriving yojson_of]
and open_description = Ppxlib__.Import.open_description [@@deriving  yojson_of]
and override_flag = Ppxlib__.Import.override_flag [@@deriving  yojson_of]
and package_type = Ppxlib__.Import.package_type [@@deriving  yojson_of]
and pattern = Ppxlib__.Import.pattern [@@deriving yojson_of]
and pattern_desc = Ppxlib__.Import.pattern_desc [@@deriving  yojson_of]
and payload = Ppxlib__.Import.payload [@@deriving yojson_of]
and position = Ppxlib__.Import.position [@@deriving  yojson_of]
and private_flag = Ppxlib__.Import.private_flag [@@deriving  yojson_of]
and row_field = Ppxlib__.Import.row_field [@@deriving  yojson_of]
and row_field_desc = Ppxlib__.Import.row_field_desc [@@deriving  yojson_of]
and signature = Ppxlib__.Import.signature [@@deriving  yojson_of]
and signature_item = Ppxlib__.Import.signature_item [@@deriving  yojson_of]
and signature_item_desc = Ppxlib__.Import.signature_item_desc [@@deriving  yojson_of]
and structure = Ppxlib__.Import.structure [@@deriving  yojson_of]
and structure_item = Ppxlib__.Import.structure_item [@@deriving  yojson_of]
and structure_item_desc = Ppxlib__.Import.structure_item_desc [@@deriving  yojson_of]
and type_declaration = Ppxlib__.Import.type_declaration [@@deriving  yojson_of]
and type_exception = Ppxlib__.Import.type_exception [@@deriving  yojson_of]
and type_extension = Ppxlib__.Import.type_extension [@@deriving  yojson_of]
and type_kind = Ppxlib__.Import.type_kind [@@deriving  yojson_of]
and value_binding = Ppxlib__.Import.value_binding [@@deriving  yojson_of]
and value_description = Ppxlib__.Import.value_description [@@deriving  yojson_of]
and variance = Ppxlib__.Import.variance [@@deriving  yojson_of]
and virtual_flag = Ppxlib__.Import.virtual_flag [@@deriving  yojson_of]
and with_constraint = Ppxlib__.Import.with_constraint [@@deriving  yojson_of]
 ]

let print_yojson json =
  
  let s= Yojson.Safe.pretty_to_string ~std:true json in
  let s2 = "Hello, World!" ^ s in 
  print_endline s2

let transform_one_signature (item:Ppxlib__.Import.signature) =
  (print_yojson (yojson_of_signature item));
  item


let transform_one_structure (item:Ppxlib__.Import.structure) =
  (print_yojson (yojson_of_structure item));
  item

let () =
  Driver.register_transformation 
    ~impl:transform_one_structure
    ~intf:transform_one_signature
    "my_transformation"

