type role = [ `System | `User | `Assistant ]

type part = Text of string

type message = {
  role : role;
  parts : part list;
}

type config = {
  temperature : float option;
  max_tokens : int option;
  top_p : float option;
  stop : string list;
}

type t = {
  model : string option;
  messages : message list;
  config : config;
  metadata : Yojson.Safe.t option;
}

let default_config = { temperature = None; max_tokens = None; top_p = None; stop = [] }
let text role value = { role; parts = [ Text value ] }
let make ?model ?(config = default_config) ?metadata messages = { model; messages; config; metadata }

let message_text message =
  message.parts
  |> List.map (function Text text -> text)
  |> String.concat ""
