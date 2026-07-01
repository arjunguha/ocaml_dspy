let () = Mirage_crypto_rng_unix.use_default ()

let model = "gpt-5-nano"

let role_to_string = function
  | `System -> "system"
  | `User -> "user"
  | `Assistant -> "assistant"

let assoc_opt key value fields =
  match value with
  | None -> fields
  | Some value -> (key, value) :: fields

let request_to_yojson ~model (request : Lm_request.t) =
  let open Yojson.Safe in
  let messages =
    request.messages
    |> List.map (fun (message : Lm_request.message) ->
         `Assoc
           [
             ("role", `String (role_to_string message.role));
             ("content", `String (Lm_request.message_text message));
           ])
  in
  let fields =
    [
      ("model", `String (Option.value request.model ~default:model));
      ("messages", `List messages);
    ]
  in
  let fields = assoc_opt "temperature" (Option.map (fun f -> `Float f) request.config.temperature) fields in
  let fields = assoc_opt "max_tokens" (Option.map (fun i -> `Int i) request.config.max_tokens) fields in
  let fields = assoc_opt "top_p" (Option.map (fun f -> `Float f) request.config.top_p) fields in
  let fields =
    match request.config.stop with
    | [] -> fields
    | stops -> ("stop", `List (List.map (fun s -> `String s) stops)) :: fields
  in
  let fields = assoc_opt "metadata" request.metadata fields in
  `Assoc (List.rev fields)

let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let int_member name json =
  match member name json with
  | Some (`Int i) -> Some i
  | Some (`Intlit s) -> (try Some (int_of_string s) with Failure _ -> None)
  | _ -> None

let string_member name json =
  match member name json with
  | Some (`String s) -> Some s
  | _ -> None

let response_of_yojson json =
  let choices =
    match member "choices" json with
    | Some (`List []) -> Dspy_error.errorf "OpenAI response choices array is empty"
    | Some (`List choices) -> choices
    | _ -> Dspy_error.errorf "OpenAI response missing choices array"
  in
  let outputs, finish_reason =
    choices
    |> List.fold_left
         (fun (outputs, finish_reason) choice ->
           let message =
             match member "message" choice with
             | Some (`Assoc _ as message) -> message
             | _ -> Dspy_error.errorf "OpenAI response choice missing message object"
           in
           let role = string_member "role" message in
           let content =
             match string_member "content" message with
             | Some content -> content
             | None -> Dspy_error.errorf "OpenAI response message missing text content"
           in
           let finish_reason =
             match finish_reason with
             | Some _ -> finish_reason
             | None -> string_member "finish_reason" choice
           in
           ({ Lm_response.role; parts = [ { Lm_response.text = content } ] } :: outputs, finish_reason))
         ([], None)
  in
  let usage =
    match member "usage" json with
    | None | Some `Null -> None
    | Some (`Assoc _ as usage) ->
        Some
          {
            Lm_response.prompt_tokens = int_member "prompt_tokens" usage;
            completion_tokens = int_member "completion_tokens" usage;
            total_tokens = int_member "total_tokens" usage;
          }
    | Some _ -> Dspy_error.errorf "OpenAI response usage must be an object"
  in
  { Lm_response.outputs = List.rev outputs; usage; finish_reason; metadata = Some json }

let response_of_string text =
  try response_of_yojson (Yojson.Safe.from_string text) with
  | Yojson.Json_error msg ->
      Dspy_error.errorf "OpenAI-compatible chat response was not valid JSON: %s" msg

let base_url () =
  Sys.getenv_opt "OPENAI_API_BASE" |> Option.value ~default:"http://localhost:4000/v1"

let api_key () =
  match Sys.getenv_opt "OPENAI_API_KEY" with
  | Some key when String.trim key <> "" -> Some key
  | _ -> None

let endpoint () =
  let base = base_url () in
  let base = if String.ends_with ~suffix:"/" base then String.sub base 0 (String.length base - 1) else base in
  Uri.of_string (base ^ "/chat/completions")

let tls_authenticator () =
  match Ca_certs.authenticator () with
  | Ok authenticator -> authenticator
  | Error (`Msg msg) -> Dspy_error.errorf "could not load system CA certificates: %s" msg

let tls_host uri =
  match Uri.host uri with
  | None -> Dspy_error.errorf "HTTPS URI has no host: %s" (Uri.to_string uri)
  | Some hostname -> (
      try Domain_name.(host_exn (of_string_exn hostname)) with
      | Invalid_argument msg ->
          Dspy_error.errorf "invalid HTTPS host %S: %s" hostname msg)

let https_wrapper uri flow =
  let host = tls_host uri in
  let authenticator = tls_authenticator () in
  let config =
    match Tls.Config.client ~authenticator ~peer_name:host () with
    | Ok config -> config
    | Error (`Msg msg) -> Dspy_error.errorf "could not create TLS client config: %s" msg
  in
  Tls_eio.client_of_flow config ~host flow

let cohttp_post ~sw ~env ~body uri =
  let headers =
    match api_key () with
    | None -> [ ("content-type", "application/json") ]
    | Some key ->
        [
          ("content-type", "application/json");
          ("authorization", "Bearer " ^ key);
        ]
  in
  let headers =
    Cohttp.Header.of_list headers
  in
  let client = Cohttp_eio.Client.make ~https:(Some https_wrapper) (Eio.Stdenv.net env) in
  let response, response_body =
    Cohttp_eio.Client.post client ~sw ~headers ~body:(Cohttp_eio.Body.of_string body) uri
  in
  let response_text = Eio.Flow.read_all response_body in
  match Cohttp.Response.status response with
  | #Cohttp.Code.success_status -> response_text
  | status ->
      Dspy_error.errorf "OpenAI-compatible chat request failed with %s: %s"
        (Cohttp.Code.string_of_status status) response_text

let forward ~sw ~env request =
  let body = request_to_yojson ~model request |> Yojson.Safe.to_string in
  let uri = endpoint () in
  let response_text = cohttp_post ~sw ~env ~body uri in
  response_of_string response_text
