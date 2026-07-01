open Ppxlib
open Ast_builder.Default

type field = {
  name : string;
  typ : core_type;
  codec : expression;
  desc : string option;
  prefix : string option;
}

type signature_info = {
  input_decl : type_declaration;
  output_decl : type_declaration;
  inputs : field list;
  outputs : field list;
}

let lident ~loc name = Located.mk ~loc (Longident.Lident name)

let lid ~loc path =
  let build = function
    | [] -> invalid_arg "empty longident"
    | first :: rest ->
        List.fold_left
          (fun acc name -> Longident.Ldot (acc, name))
          (Longident.Lident first) rest
  in
  Located.mk ~loc (build path)

let epath ~loc path = pexp_ident ~loc (lid ~loc path)
let pvar' ~loc name = pvar ~loc name
let evar' ~loc name = evar ~loc name
let str ~loc value = estring ~loc value

let string_payload = function
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ({ pexp_desc = Pexp_constant (Pconst_string (value, _, _)); _ }, _);
          _;
        };
      ] ->
      Some value
  | _ -> None

let attr_string name attrs =
  attrs
  |> List.find_map (fun attr ->
       if String.equal attr.attr_name.txt name then string_payload attr.attr_payload else None)

let rec longident_last = function
  | Longident.Lident name -> name
  | Ldot (_, name) -> name
  | Lapply (_, rhs) -> longident_last rhs

let rec codec_of_type loc typ =
  match typ.ptyp_desc with
  | Ptyp_constr ({ txt; _ }, []) -> (
      match longident_last txt with
      | "string" -> [%expr Ocaml_dspy.Type.string]
      | "int" -> [%expr Ocaml_dspy.Type.int]
      | "float" -> [%expr Ocaml_dspy.Type.float]
      | "bool" -> [%expr Ocaml_dspy.Type.bool]
      | "json" -> [%expr Ocaml_dspy.Type.json]
      | "t" -> (
          match txt with
          | Ldot (Ldot (Lident "Yojson", "Safe"), "t") -> [%expr Ocaml_dspy.Type.json]
          | _ -> Location.raise_errorf ~loc "unsupported DSPy field type")
      | _ -> Location.raise_errorf ~loc "unsupported DSPy field type")
  | Ptyp_constr ({ txt; _ }, [ item ]) when String.equal (longident_last txt) "list" ->
      let item_codec = codec_of_type loc item in
      [%expr Ocaml_dspy.Type.list [%e item_codec]]
  | _ -> Location.raise_errorf ~loc "unsupported DSPy field type"

let strip_label_attrs label = { label with pld_attributes = [] }

let strip_type_decl_attrs decl =
  let ptype_kind =
    match decl.ptype_kind with
    | Ptype_record labels -> Ptype_record (List.map strip_label_attrs labels)
    | kind -> kind
  in
  { decl with ptype_attributes = []; ptype_kind }

let field_of_label label =
  {
    name = label.pld_name.txt;
    typ = label.pld_type;
    codec = codec_of_type label.pld_loc label.pld_type;
    desc = attr_string "desc" label.pld_attributes;
    prefix = attr_string "prefix" label.pld_attributes;
  }

let fields_of_type_decl loc decl =
  match decl.ptype_kind with
  | Ptype_record labels -> List.map field_of_label labels
  | _ -> Location.raise_errorf ~loc "DSPy signature types must be records"

let reject_duplicate_fields loc inputs outputs =
  let seen = Hashtbl.create 16 in
  let check kind field =
    match Hashtbl.find_opt seen field.name with
    | None -> Hashtbl.add seen field.name kind
    | Some previous ->
        Location.raise_errorf ~loc "duplicate DSPy field %S in signature (%s and %s)"
          field.name previous kind
  in
  List.iter (check "input") inputs;
  List.iter (check "output") outputs

let parse_signature_payload loc payload =
  match payload with
  | PStr items ->
      let instructions =
        match items with
        | {
            pstr_desc =
              Pstr_eval
                ({ pexp_desc = Pexp_constant (Pconst_string (value, _, _)); _ }, _);
            _;
          }
          :: _ ->
            value
        | _ -> Location.raise_errorf ~loc "%%dspy.signature requires an instruction string first"
      in
      let find_type name =
        items
        |> List.find_map (function
             | { pstr_desc = Pstr_type (_, decls); _ } ->
                 List.find_opt (fun decl -> String.equal decl.ptype_name.txt name) decls
             | _ -> None)
      in
      let input_decl =
        match find_type "t_in" with
        | Some decl -> decl
        | None -> Location.raise_errorf ~loc "%%dspy.signature requires type t_in"
      in
      let output_decl =
        match find_type "t_out" with
        | Some decl -> decl
        | None -> Location.raise_errorf ~loc "%%dspy.signature requires type t_out"
      in
      let inputs = fields_of_type_decl loc input_decl in
      let outputs = fields_of_type_decl loc output_decl in
      reject_duplicate_fields loc inputs outputs;
      ( instructions,
        {
          input_decl = strip_type_decl_attrs input_decl;
          output_decl = strip_type_decl_attrs output_decl;
          inputs;
          outputs;
        } )
  | _ -> Location.raise_errorf ~loc "%%dspy.signature payload must be a structure"

let apply ?desc ?prefix ~loc fn name codec =
  let args =
    []
    |> (fun args ->
         match prefix with
         | None -> args
         | Some prefix -> (Labelled "prefix", str ~loc prefix) :: args)
    |> (fun args ->
         match desc with
         | None -> args
         | Some desc -> (Labelled "desc", str ~loc desc) :: args)
  in
  pexp_apply ~loc fn (List.rev_append args [ (Nolabel, str ~loc name); (Nolabel, codec) ])

let value_binding' ~loc name expr =
  value_binding ~loc ~pat:(pvar' ~loc name) ~expr

let witness ~loc kind field =
  let signature_kind =
    ptyp_constr ~loc (lid ~loc [ "Ocaml_dspy"; "Signature"; kind ]) [ field.typ ]
  in
  let expr =
    apply ?desc:field.desc ?prefix:field.prefix ~loc
      (epath ~loc [ "Ocaml_dspy"; "Signature"; kind ])
      field.name field.codec
  in
  let expr = pexp_constraint ~loc expr signature_kind in
  pstr_value ~loc Nonrecursive [ value_binding' ~loc field.name expr ]

let packed ~loc constructor field =
  pexp_construct ~loc (lid ~loc [ "Ocaml_dspy"; "Signature"; constructor ])
    (Some (evar' ~loc field.name))

let list_expr ~loc values = elist ~loc values

let record_get ~loc record_name field =
  pexp_field ~loc (evar' ~loc record_name) (lident ~loc field.name)

let encode_expr ~loc fields =
  List.fold_left
    (fun acc field ->
      [%expr
        Ocaml_dspy.Values.set [%e evar' ~loc field.name]
          [%e record_get ~loc "x" field] [%e acc]])
    [%expr Ocaml_dspy.Values.empty] fields

let decode_record ~loc fields =
  let fields =
    fields
    |> List.map (fun field ->
         ( lident ~loc field.name,
           [%expr Ocaml_dspy.Prediction.get [%e evar' ~loc field.name] p] ))
  in
  pexp_record ~loc fields None

let make_input_expr ~loc fields =
  let record_fields =
    fields |> List.map (fun field -> (lident ~loc field.name, evar' ~loc field.name))
  in
  let body = pexp_record ~loc record_fields None in
  List.fold_right
    (fun field acc -> pexp_fun ~loc (Labelled field.name) None (pvar' ~loc field.name) acc)
    fields body

let dspy_call_expr ~loc fields =
  let record_fields =
    fields |> List.map (fun field -> (lident ~loc field.name, evar' ~loc field.name))
  in
  let body = [%expr f [%e pexp_record ~loc record_fields None]] in
  let with_labels =
    List.fold_right
      (fun field acc -> pexp_fun ~loc (Labelled field.name) None (pvar' ~loc field.name) acc)
      fields body
  in
  pexp_fun ~loc Nolabel None (pvar' ~loc "f") with_labels

let signature_module_expr ~loc instructions info =
  let items =
    [
      pstr_type ~loc Recursive [ info.input_decl ];
      pstr_type ~loc Recursive [ info.output_decl ];
    ]
    @ List.map (witness ~loc "input") info.inputs
    @ List.map (witness ~loc "output") info.outputs
  in
  let signature_expr =
    [%expr
      Ocaml_dspy.Signature.make ~instructions:[%e str ~loc instructions]
        ~inputs:[%e list_expr ~loc (List.map (packed ~loc "Packed_input") info.inputs)]
        ~outputs:[%e list_expr ~loc (List.map (packed ~loc "Packed_output") info.outputs)]
        ()]
  in
  let items =
    items
    @ [
        pstr_value ~loc Nonrecursive [ value_binding' ~loc "signature" signature_expr ];
        pstr_value ~loc Nonrecursive
          [ value_binding' ~loc "encode" (pexp_fun ~loc Nolabel None (pvar' ~loc "x") (encode_expr ~loc info.inputs)) ];
        pstr_value ~loc Nonrecursive
          [
            value_binding' ~loc "decode"
              (pexp_fun ~loc Nolabel None (pvar' ~loc "p")
                 (pexp_constraint ~loc (decode_record ~loc info.outputs) [%type: t_out]));
          ];
        [%stri
          module Dspy_internal = struct
            let make_input = [%e make_input_expr ~loc info.inputs]
            let call = [%e dspy_call_expr ~loc info.inputs]
          end];
      ]
  in
  pmod_structure ~loc items

let parse_predict_payload loc payload =
  match payload with
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ({ pexp_desc = Pexp_apply ({ pexp_desc = Pexp_pack module_expr; _ }, args); _ }, _);
          _;
        };
      ] ->
      let lm =
        args
        |> List.find_map (function
             | Labelled "lm", expr -> Some expr
             | _ -> None)
      in
      let lm =
        match lm with
        | Some lm -> lm
        | None -> Location.raise_errorf ~loc "%%dspy.predict requires ~lm"
      in
      (module_expr, lm)
  | _ -> Location.raise_errorf ~loc "expected [%%dspy.predict (module S) ~lm]"

let predict_module_expr ~loc module_expr lm_expr =
  pmod_structure ~loc
    [
      [%stri module Signature_module = [%m module_expr]];
      [%stri
        module Dspy_internal = struct
          let predict =
            Ocaml_dspy.Predict.create ~lm:[%e lm_expr]
              Signature_module.signature
        end];
      [%stri
        let forward ~sw ~env (input : Signature_module.t_in) :
            Signature_module.t_out =
          let prediction =
            Ocaml_dspy.Predict.forward ~sw ~env Dspy_internal.predict
              (Signature_module.encode input)
          in
          Signature_module.decode prediction];
      [%stri let call ~sw ~env = Signature_module.Dspy_internal.call (forward ~sw ~env)];
    ]

let cot_module_expr ~loc module_expr lm_expr =
  pmod_structure ~loc
    [
      [%stri module Signature_module = [%m module_expr]];
      [%stri type result = { reasoning : string; output : Signature_module.t_out }];
      [%stri
        module Dspy_internal = struct
          let cot =
            Ocaml_dspy.Chain_of_thought.create ~lm:[%e lm_expr]
              Signature_module.signature
        end];
      [%stri
        let forward ~sw ~env (input : Signature_module.t_in) : result =
          let core_result : Ocaml_dspy.Chain_of_thought.result =
            Ocaml_dspy.Chain_of_thought.forward ~sw ~env Dspy_internal.cot
              (Signature_module.encode input)
          in
          {
            reasoning = core_result.reasoning;
            output = Signature_module.decode core_result.prediction;
          }];
      [%stri let call ~sw ~env = Signature_module.Dspy_internal.call (forward ~sw ~env)];
    ]

class mapper =
  object (self)
    inherit Ast_traverse.map as super

    method! structure_item item =
      match item.pstr_desc with
      | Pstr_module binding -> (
          match binding.pmb_expr.pmod_desc with
          | Pmod_extension ({ txt = "dspy.signature"; loc }, payload) ->
              let instructions, info = parse_signature_payload loc payload in
              let pmb_expr = signature_module_expr ~loc instructions info in
              { item with pstr_desc = Pstr_module { binding with pmb_expr } }
          | Pmod_extension ({ txt = "dspy.predict"; loc }, payload) ->
              let module_expr, lm_expr = parse_predict_payload loc payload in
              let pmb_expr = predict_module_expr ~loc module_expr lm_expr in
              { item with pstr_desc = Pstr_module { binding with pmb_expr } }
          | Pmod_extension ({ txt = "dspy.chain_of_thought"; loc }, payload) ->
              let module_expr, lm_expr = parse_predict_payload loc payload in
              let pmb_expr = cot_module_expr ~loc module_expr lm_expr in
              { item with pstr_desc = Pstr_module { binding with pmb_expr } }
          | _ -> super#structure_item item)
      | _ -> super#structure_item item
  end

let map_structure structure = (new mapper)#structure structure

let () =
  Driver.register_transformation "ppx_ocaml_dspy" ~impl:map_structure
