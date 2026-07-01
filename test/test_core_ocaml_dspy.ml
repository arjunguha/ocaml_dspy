open Ocaml_dspy

let question : string Signature.input =
  Signature.input ~desc:"Question" "question" Type.string

let answer : string Signature.output =
  Signature.output ~desc:"Answer" "answer" Type.string

let qa_signature =
  Signature.make ~instructions:"Answer concisely."
    ~inputs:[ Signature.Packed_input question ]
    ~outputs:[ Signature.Packed_output answer ]
    ()

let test_signature () =
  (* This test inspects the hand-built QA signature used by the core suite.
     It verifies that the configured instructions, single input, and single
     output are preserved, and that the question input keeps its name and
     description metadata. *)
  Alcotest.(check string) "instructions" "Answer concisely." (Signature.instructions qa_signature);
  Alcotest.(check int) "one input" 1 (List.length (Signature.inputs qa_signature));
  Alcotest.(check int) "one output" 1 (List.length (Signature.outputs qa_signature));
  Alcotest.(check string) "input name" "question" (Signature.input_name question);
  Alcotest.(check (option string)) "input desc" (Some "Question") (Signature.input_field question).desc

let test_values_prediction () =
  (* This test builds a Values map containing the QA question, validates it
     against the QA signature, and then builds a Prediction map containing the
     answer. It expects the value lookup and prediction lookup to return the
     exact strings that were inserted, with validation accepting the complete
     input set. *)
  let values = Values.empty |> Values.set question "What is the capital of France?" in
  Alcotest.(check (option string)) "value" (Some "What is the capital of France?") (Values.get question values);
  Values.validate qa_signature values;
  let prediction = Prediction.empty |> Prediction.set answer "Paris" in
  Alcotest.(check string) "prediction" "Paris" (Prediction.get answer prediction)

let test_type_list () =
  (* This test exercises the list type wrapper with string elements. It renders
     a two-item OCaml list to the JSON representation used in prompts, then
     parses that rendered text back, expecting a successful round trip with the
     original element order intact. *)
  let typ = Type.list Type.string in
  let rendered = Type.render typ [ "alpha"; "beta" ] in
  Alcotest.(check string) "list renders JSON" "[\"alpha\",\"beta\"]" rendered;
  Alcotest.(check (result (list string) string))
    "list parses rendered JSON" (Ok [ "alpha"; "beta" ]) (Type.parse typ rendered)

let test_chat_adapter () =
  (* This test renders the QA signature and question values through the default
     chat adapter, then parses a synthetic LM response containing the expected
     answer marker. It checks that rendering emits a system/user message pair,
     parsing extracts "Paris", and the system prompt begins with the signature
     instructions. *)
  let values = Values.empty |> Values.set question "What is the capital of France?" in
  let messages = Chat_adapter.render Chat_adapter.default qa_signature values in
  Alcotest.(check int) "message count" 2 (List.length messages);
  let prediction =
    Chat_adapter.parse Chat_adapter.default qa_signature
      "Some preface\n[[ ## answer ## ]]\nParis\n"
  in
  Alcotest.(check string) "parsed" "Paris" (Prediction.get answer prediction);
  let system_message =
    match messages with
    | system_message :: _ -> Lm_request.message_text system_message
    | [] -> Alcotest.fail "adapter rendered no messages"
  in
  Alcotest.(check bool) "prompt mentions instructions" true
    (String.starts_with ~prefix:"Answer concisely." system_message)

let test_openai_json () =
  (* This test covers the OpenAI-compatible JSON boundary without making a
     network call. It first confirms that an explicit request model overrides
     the default model during JSON conversion, then decodes a minimal successful
     chat response and expects the assistant text and finish reason to be
     preserved. *)
  let request =
    Lm_request.make ~model:"test-model"
      [ Lm_request.text `User "Hello" ]
  in
  let json = Openai_chat_lm.request_to_yojson ~model:"default" request in
  Alcotest.(check string) "request model" "test-model" (Yojson.Safe.Util.(json |> member "model" |> to_string));
  let response =
    Openai_chat_lm.response_of_yojson
      (`Assoc
        [
          ( "choices",
            `List
              [
                `Assoc
                  [
                    ( "message",
                      `Assoc [ ("role", `String "assistant"); ("content", `String "Hi") ] );
                    ("finish_reason", `String "stop");
                  ];
              ] );
          ( "usage",
            `Assoc
              [
                ("prompt_tokens", `Int 1);
                ("completion_tokens", `Int 2);
                ("total_tokens", `Int 3);
              ] );
        ])
  in
  Alcotest.(check string) "response text" "Hi" (Lm_response.text response);
  Alcotest.(check (option string)) "finish" (Some "stop") response.finish_reason

let () =
  Alcotest.run "ocaml_dspy_core"
    [
      ( "core",
        [
          Alcotest.test_case "signature" `Quick test_signature;
          Alcotest.test_case "values and prediction" `Quick test_values_prediction;
          Alcotest.test_case "list type" `Quick test_type_list;
        ] );
      ("adapter", [ Alcotest.test_case "render and parse" `Quick test_chat_adapter ]);
      ("openai-json", [ Alcotest.test_case "json conversion" `Quick test_openai_json ]);
    ]
