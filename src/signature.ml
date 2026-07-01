type field = {
  name : string;
  type_name : string;
  desc : string option;
  prefix : string option;
}

type 'a input = {
  field : field;
  typ : 'a Type.t;
  key : 'a Hmap.key;
}

type 'a output = {
  field : field;
  typ : 'a Type.t;
  key : 'a Hmap.key;
}

type packed_input = Packed_input : 'a input -> packed_input
type packed_output = Packed_output : 'a output -> packed_output

type t = {
  instructions : string;
  inputs : packed_input list;
  outputs : packed_output list;
}

let make_field ?desc ?prefix name typ =
  if String.trim name = "" then Dspy_error.errorf "field name cannot be empty";
  { name; type_name = Type.name typ; desc; prefix }

let input ?desc ?prefix name typ : 'a input =
  { field = make_field ?desc ?prefix name typ; typ; key = Hmap.Key.create () }

let output ?desc ?prefix name typ : 'a output =
  { field = make_field ?desc ?prefix name typ; typ; key = Hmap.Key.create () }

let field_name = function
  | `Input (Packed_input i) -> i.field.name
  | `Output (Packed_output o) -> o.field.name

let reject_duplicates inputs outputs =
  let seen = Hashtbl.create 16 in
  let check kind name =
    match Hashtbl.find_opt seen name with
    | None -> Hashtbl.add seen name kind
    | Some previous ->
        Dspy_error.errorf "duplicate field %S in signature (%s and %s)" name previous kind
  in
  List.iter (fun input -> check "input" (field_name (`Input input))) inputs;
  List.iter (fun output -> check "output" (field_name (`Output output))) outputs

let make ~instructions ~inputs ~outputs () =
  reject_duplicates inputs outputs;
  { instructions; inputs; outputs }

let instructions t = t.instructions
let with_instructions instructions t = { t with instructions }

let append_instructions more t =
  let instructions =
    if String.trim t.instructions = "" then more
    else if String.trim more = "" then t.instructions
    else t.instructions ^ "\n" ^ more
  in
  { t with instructions }

let inputs t = t.inputs
let outputs t = t.outputs

let input_field (input : 'a input) = input.field
let output_field (output : 'a output) = output.field
let input_type (input : 'a input) = input.typ
let output_type (output : 'a output) = output.typ
let input_name (input : 'a input) = input.field.name
let output_name (output : 'a output) = output.field.name

module Private = struct
  type hidden_key = Hmap.Key.t
  type map = Hmap.t

  let equal_hidden_key = Hmap.Key.equal
  let input_hidden_key (input : 'a input) = Hmap.Key.hide_type input.key
  let output_hidden_key (output : 'a output) = Hmap.Key.hide_type output.key
  let empty_map = Hmap.empty
  let add_input (input : 'a input) value map = Hmap.add input.key value map
  let find_input (input : 'a input) map = Hmap.find input.key map
  let mem_input (input : 'a input) map = Hmap.mem input.key map
  let add_output (output : 'a output) value map = Hmap.add output.key value map
  let find_output (output : 'a output) map = Hmap.find output.key map
end
