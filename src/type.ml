type 'a t = {
  name : string;
  render : 'a -> string;
  parse : string -> ('a, string) result;
  to_json : 'a -> Yojson.Safe.t;
  of_json : Yojson.Safe.t -> ('a, string) result;
}

let name t = t.name
let render t v = t.render v
let parse t s = t.parse s

let parse_exn ~field t s =
  match parse t s with
  | Ok v -> v
  | Error msg -> Dspy_error.errorf "could not parse field %S as %s: %s" field t.name msg

let to_json t v = t.to_json v
let of_json t json = t.of_json json
let make ~name ~render ~parse ~to_json ~of_json = { name; render; parse; to_json; of_json }

let trim = String.trim

let string =
  make ~name:"string" ~render:Fun.id ~parse:(fun s -> Ok s)
    ~to_json:(fun s -> `String s)
    ~of_json:(function
      | `String s -> Ok s
      | json -> Error (Printf.sprintf "expected JSON string, got %s" (Yojson.Safe.to_string json)))

let int =
  make ~name:"int" ~render:string_of_int
    ~parse:(fun s ->
      try Ok (int_of_string (trim s)) with Failure _ -> Error "expected integer")
    ~to_json:(fun i -> `Int i)
    ~of_json:(function
      | `Int i -> Ok i
      | `Intlit s -> (try Ok (int_of_string s) with Failure _ -> Error "integer literal out of range")
      | json -> Error (Printf.sprintf "expected JSON integer, got %s" (Yojson.Safe.to_string json)))

let float =
  make ~name:"float" ~render:string_of_float
    ~parse:(fun s ->
      try Ok (float_of_string (trim s)) with Failure _ -> Error "expected float")
    ~to_json:(fun f -> `Float f)
    ~of_json:(function
      | `Float f -> Ok f
      | `Int i -> Ok (float_of_int i)
      | json -> Error (Printf.sprintf "expected JSON number, got %s" (Yojson.Safe.to_string json)))

let bool =
  make ~name:"bool" ~render:string_of_bool
    ~parse:(fun s ->
      match String.lowercase_ascii (trim s) with
      | "true" -> Ok true
      | "false" -> Ok false
      | _ -> Error "expected true or false")
    ~to_json:(fun b -> `Bool b)
    ~of_json:(function
      | `Bool b -> Ok b
      | json -> Error (Printf.sprintf "expected JSON boolean, got %s" (Yojson.Safe.to_string json)))

let json =
  make ~name:"json" ~render:Yojson.Safe.pretty_to_string
    ~parse:(fun s ->
      try Ok (Yojson.Safe.from_string s) with Yojson.Json_error msg -> Error msg)
    ~to_json:Fun.id ~of_json:(fun json -> Ok json)

let list item =
  let parse_json_array s =
    try
      match Yojson.Safe.from_string s with
      | `List items ->
          let rec loop acc = function
            | [] -> Ok (List.rev acc)
            | json :: rest -> (
                match of_json item json with
                | Ok v -> loop (v :: acc) rest
                | Error msg -> Error msg)
          in
          loop [] items
      | _ -> Error "expected JSON array"
    with Yojson.Json_error msg -> Error msg
  in
  make
    ~name:("list(" ^ name item ^ ")")
    ~render:(fun values -> `List (List.map (to_json item) values) |> Yojson.Safe.to_string)
    ~parse:parse_json_array
    ~to_json:(fun values -> `List (List.map (to_json item) values))
    ~of_json:(function
      | `List items ->
          let rec loop acc = function
            | [] -> Ok (List.rev acc)
            | json :: rest -> (
                match of_json item json with
                | Ok v -> loop (v :: acc) rest
                | Error msg -> Error msg)
          in
          loop [] items
      | json -> Error (Printf.sprintf "expected JSON array, got %s" (Yojson.Safe.to_string json)))
