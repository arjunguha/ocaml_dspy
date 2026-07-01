type t = Signature.Private.map

let empty = Signature.Private.empty_map

let set output value t = Signature.Private.add_output output value t

let get_opt output t = Signature.Private.find_output output t

let get output t =
  match get_opt output t with
  | Some value -> value
  | None -> Dspy_error.errorf "missing output field %S" (Signature.output_name output)
