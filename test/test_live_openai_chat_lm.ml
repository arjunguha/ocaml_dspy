open Ocaml_dspy

module Config = struct
  type t = {
    base_url : string;
    api_key : string option;
    model : string;
  }

  let default_base_url = "http://localhost:4000/v1"
  let default_model = Openai_chat_lm.model

  let default =
    { base_url = default_base_url; api_key = None; model = default_model }

  let make base_url api_key model = { base_url; api_key; model }
end

let make_tests (config : Config.t) =
  let module Live_lm = Openai_chat_lm.Make (struct
    let base_url = config.base_url
    let api_key = config.api_key
    let model = config.model
  end) in
  let module Suite = Ocaml_dspy_test_predict_suite.Test_predict_suite.Make (struct
    let name = "live"
    let lm = (module Live_lm : Ocaml_dspy.Lm)
    let speed = `Slow

    let run f =
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw -> f ~sw ~env
  end) in
  Suite.tests

let cli_config =
  let open Cmdliner in
  let base_url =
    let doc = "OpenAI-compatible API base URL." in
    Arg.(
      value
      & opt string Config.default_base_url
      & info [ "api-base" ] ~docv:"URL" ~doc)
  in
  let api_key =
    let doc = "API key used as a Bearer token. Omit it for local gateways that do not require authentication." in
    Arg.(value & opt (some string) None & info [ "api-key" ] ~docv:"KEY" ~doc)
  in
  let model =
    let doc = "Model name to send in Chat Completions requests." in
    Arg.(
      value
      & opt string Config.default_model
      & info [ "model" ] ~docv:"MODEL" ~doc)
  in
  Term.(const Config.make $ base_url $ api_key $ model)

let configure_test_case index (name, speed, _) =
  Alcotest.test_case name speed (fun config ->
      match List.nth_opt (make_tests config) index with
      | Some (_, _, test) -> test ()
      | None -> Alcotest.failf "missing live test case at index %d" index)

let () =
  let tests =
    make_tests Config.default |> List.mapi configure_test_case
  in
  Alcotest.run_with_args ("ocaml_dspy_live_" ^ Config.default_model) cli_config
    [ ("predict", tests) ]
