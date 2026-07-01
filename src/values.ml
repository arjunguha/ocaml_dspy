type t = {
  values : Signature.Private.map;
  names : (Signature.Private.hidden_key * string) list;
}

let empty = { values = Signature.Private.empty_map; names = [] }

let add_name key name names =
  (key, name)
  :: List.filter
       (fun (existing_key, _) ->
         not (Signature.Private.equal_hidden_key existing_key key))
       names

let set input value t =
  let key = Signature.Private.input_hidden_key input in
  {
    values = Signature.Private.add_input input value t.values;
    names = add_name key (Signature.input_name input) t.names;
  }

let get input t = Signature.Private.find_input input t.values

let get_exn input t =
  match get input t with
  | Some value -> value
  | None -> Dspy_error.errorf "missing input field %S" (Signature.input_name input)

let validate signature t =
  let expected_keys =
    Signature.inputs signature
    |> List.map (fun (Signature.Packed_input input) ->
         Signature.Private.input_hidden_key input)
  in
  List.iter
    (fun (key, name) ->
      if not (List.exists (Signature.Private.equal_hidden_key key) expected_keys) then
        Dspy_error.errorf "value for field %S is not an input of this signature" name)
    t.names;
  Signature.inputs signature
  |> List.iter (fun (Signature.Packed_input input) ->
       if not (Signature.Private.mem_input input t.values) then
         Dspy_error.errorf "missing input field %S" (Signature.input_name input))
