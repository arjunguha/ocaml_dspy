open Ocaml_dspy

module Suite = Ocaml_dspy_test_predict_suite.Test_predict_suite.Make (struct
  let name = "live"
  let lm = (module Openai_chat_lm : Ocaml_dspy.Lm)
  let speed = `Slow

  let run f =
    (match (Sys.getenv_opt "OPENAI_API_BASE", Sys.getenv_opt "OPENAI_API_KEY") with
    | Some _, Some _ -> ()
    | _ -> Alcotest.skip ());
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw -> f ~sw ~env
end)

let () =
  Alcotest.run ("ocaml_dspy_live_" ^ Openai_chat_lm.model)
    [ ("predict", Suite.tests) ]
