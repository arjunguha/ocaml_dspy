open Ocaml_dspy

module Mock_lm : Ocaml_dspy.Lm = struct
  let model = "mock"

  let forward ~sw:_ ~env:_ (request : Lm_request.t) =
    let prompt =
      request.messages |> List.map Lm_request.message_text |> String.concat "\n"
    in
    let has_marker marker =
      prompt |> String.split_on_char '\n' |> List.exists (String.equal marker)
    in
    if has_marker "[[ ## text_out ## ]]" then
      Lm_response.of_text
        {|
[[ ## text_out ## ]]
typed string

[[ ## int_out ## ]]
17

[[ ## float_out ## ]]
2.75

[[ ## bool_out ## ]]
true

[[ ## json_out ## ]]
{"kind":"object","ok":true}

[[ ## string_list_out ## ]]
["red","blue"]

[[ ## int_list_out ## ]]
[1,2,3]

[[ ## float_list_out ## ]]
[0.5,1.25]

[[ ## bool_list_out ## ]]
[true,false]

[[ ## json_list_out ## ]]
[{"name":"first"},{"name":"second"}]
|}
    else if has_marker "[[ ## numeric_answer ## ]]" then
      Lm_response.of_text "[[ ## numeric_answer ## ]]\nnot a number"
    else if has_marker "[[ ## reasoning ## ]]" then
      Lm_response.of_text
        "[[ ## reasoning ## ]]\nBecause 2 + 2 is 4.\n\n[[ ## answer ## ]]\n4"
    else if has_marker "[[ ## answer ## ]]" then
      Lm_response.of_text "[[ ## answer ## ]]\n4"
    else Lm_response.of_text "4"
end

module Suite = Ocaml_dspy_test_predict_suite.Test_predict_suite.Make (struct
  let name = "mock"
  let lm = (module Mock_lm : Ocaml_dspy.Lm)
  let speed = `Quick

  let run f =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw -> f ~sw ~env
end)

let () = Alcotest.run "ocaml_dspy_mock" [ ("predict", Suite.tests) ]
