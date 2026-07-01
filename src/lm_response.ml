type text_part = { text : string }

type output = {
  role : string option;
  parts : text_part list;
}

type usage = {
  prompt_tokens : int option;
  completion_tokens : int option;
  total_tokens : int option;
}

type t = {
  outputs : output list;
  usage : usage option;
  finish_reason : string option;
  metadata : Yojson.Safe.t option;
}

let of_text text =
  { outputs = [ { role = Some "assistant"; parts = [ { text } ] } ]; usage = None; finish_reason = None; metadata = None }

let text t =
  t.outputs
  |> List.map (fun output -> output.parts |> List.map (fun part -> part.text) |> String.concat "")
  |> String.concat "\n"
