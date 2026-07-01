type t = unit

let default = ()

let field_line field =
  let desc =
    match field.Signature.desc with
    | None -> ""
    | Some desc -> " - " ^ desc
  in
  Printf.sprintf "- %s (%s)%s" field.name field.type_name desc

let render () signature values =
  Values.validate signature values;
  let input_docs =
    Signature.inputs signature
    |> List.map (fun (Signature.Packed_input input) -> field_line (Signature.input_field input))
    |> String.concat "\n"
  in
  let output_docs =
    Signature.outputs signature
    |> List.map (fun (Signature.Packed_output output) -> field_line (Signature.output_field output))
    |> String.concat "\n"
  in
  let markers =
    Signature.outputs signature
    |> List.map (fun (Signature.Packed_output output) ->
         Printf.sprintf "[[ ## %s ## ]]\n<%s>" (Signature.output_name output) (Signature.output_name output))
    |> String.concat "\n\n"
  in
  let system =
    String.concat "\n\n"
      [
        Signature.instructions signature;
        "Input fields:\n" ^ input_docs;
        "Output fields:\n" ^ output_docs;
        "Respond only with each output field under its marker, in this structure:\n" ^ markers;
      ]
  in
  let input_text =
    Signature.inputs signature
    |> List.map (fun (Signature.Packed_input input) ->
         let value = Values.get_exn input values |> Type.render (Signature.input_type input) in
         let prefix =
           match (Signature.input_field input).prefix with
           | None -> Signature.input_name input
           | Some prefix -> prefix
         in
         Printf.sprintf "[[ ## %s ## ]]\n%s" prefix value)
    |> String.concat "\n\n"
  in
  [ Lm_request.text `System system; Lm_request.text `User input_text ]

let marker_name line =
  let line = String.trim line in
  let prefix = "[[ ## " in
  let suffix = " ## ]]" in
  let prefix_len = String.length prefix in
  let suffix_len = String.length suffix in
  let len = String.length line in
  if len >= prefix_len + suffix_len
     && String.sub line 0 prefix_len = prefix
     && String.sub line (len - suffix_len) suffix_len = suffix
  then Some (String.sub line prefix_len (len - prefix_len - suffix_len) |> String.trim)
  else None

let parse_sections ~allowed text =
  let table = Hashtbl.create 8 in
  let current = ref None in
  let buffer = Buffer.create 128 in
  let is_allowed name = List.exists (String.equal name) allowed in
  let flush () =
    match !current with
    | None -> Buffer.clear buffer
    | Some name ->
        if Hashtbl.mem table name then
          Dspy_error.errorf "LM response contains duplicate output marker for field %S" name;
        Hashtbl.add table name (Buffer.contents buffer |> String.trim);
        Buffer.clear buffer
  in
  String.split_on_char '\n' text
  |> List.iter (fun line ->
       match marker_name line with
       | Some name ->
           if not (is_allowed name) then
             Dspy_error.errorf "LM response contains unknown output marker %S" name;
           flush ();
           current := Some name
       | None ->
           if Option.is_some !current then (
             if Buffer.length buffer > 0 then Buffer.add_char buffer '\n';
             Buffer.add_string buffer line));
  flush ();
  table

let parse () signature text =
  let allowed =
    Signature.outputs signature
    |> List.map (fun (Signature.Packed_output output) -> Signature.output_name output)
  in
  let sections = parse_sections ~allowed text in
  Signature.outputs signature
  |> List.fold_left
       (fun prediction (Signature.Packed_output output) ->
         let name = Signature.output_name output in
         let raw =
           match Hashtbl.find_opt sections name with
           | Some raw -> raw
           | None -> Dspy_error.errorf "LM response missing output marker for field %S" name
         in
         let value = Type.parse_exn ~field:name (Signature.output_type output) raw in
         Prediction.set output value prediction)
       Prediction.empty
