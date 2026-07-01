open Ocaml_dspy

module QA = [%dspy.signature
  "Answer."

  type t_in = {
    question : string;
  }

  type t_out = {
    answer : string;
  }
]

module IntQA = [%dspy.signature
  "Answer."

  type t_in = {
    question : string;
  }

  type t_out = {
    answer : int;
  }
]

module ReasoningCollision = [%dspy.signature
  "Answer."

  type t_in = {
    question : string;
  }

  type t_out = {
    reasoning : string;
    answer : string;
  }
]

let test_signature_input_rejects_empty_name () =
  (* This test constructs an input field with only whitespace as its name. It
     verifies that Signature.input normalizes/checks field names immediately and
     raises the field-name validation error instead of creating an unusable
     input descriptor. *)
  Alcotest.check_raises "empty input name"
    (Dspy_error.Error "field name cannot be empty")
    (fun () -> ignore (Signature.input "  " Type.string : string Signature.input))

let test_signature_output_rejects_empty_name () =
  (* This test constructs an output field with an empty name. It expects
     Signature.output to reject the descriptor at construction time with the
     same field-name validation error used for invalid inputs. *)
  Alcotest.check_raises "empty output name"
    (Dspy_error.Error "field name cannot be empty")
    (fun () -> ignore (Signature.output "" Type.string : string Signature.output))

let test_signature_make_rejects_duplicate_inputs () =
  (* This test creates two input descriptors that share the field name "field"
     and tries to put both into one signature. It expects Signature.make to
     reject the duplicate during signature assembly and report an input/input
     collision. *)
  Alcotest.check_raises "duplicate inputs"
    (Dspy_error.Error "duplicate field \"field\" in signature (input and input)")
    (fun () ->
      let first : string Signature.input = Signature.input "field" Type.string in
      let second : int Signature.input = Signature.input "field" Type.int in
      ignore
        (Signature.make ~instructions:"Duplicate inputs"
           ~inputs:[ Signature.Packed_input first; Signature.Packed_input second ]
           ~outputs:[ Signature.Packed_output QA.answer ]
           ()))

let test_signature_make_rejects_duplicate_outputs () =
  (* This test creates two output descriptors with the same field name and uses
     them in a single signature. It verifies that Signature.make detects the
     duplicate output names and raises the output/output collision error. *)
  Alcotest.check_raises "duplicate outputs"
    (Dspy_error.Error "duplicate field \"field\" in signature (output and output)")
    (fun () ->
      let first : string Signature.output = Signature.output "field" Type.string in
      let second : int Signature.output = Signature.output "field" Type.int in
      ignore
        (Signature.make ~instructions:"Duplicate outputs"
           ~inputs:[ Signature.Packed_input QA.question ]
           ~outputs:[ Signature.Packed_output first; Signature.Packed_output second ]
           ()))

let test_signature_make_rejects_duplicate_input_output () =
  (* This test builds one input and one output descriptor that both use the name
     "field". It expects Signature.make to reject a signature whose input and
     output namespaces overlap, producing the input/output duplicate-field
     error. *)
  Alcotest.check_raises "duplicate input and output"
    (Dspy_error.Error "duplicate field \"field\" in signature (input and output)")
    (fun () ->
      let input : string Signature.input = Signature.input "field" Type.string in
      let output : string Signature.output = Signature.output "field" Type.string in
      ignore
        (Signature.make ~instructions:"Duplicate input and output"
           ~inputs:[ Signature.Packed_input input ]
           ~outputs:[ Signature.Packed_output output ]
           ()))

let test_values_get_exn_rejects_missing_input () =
  (* This test reads the QA question input from an empty Values map using the
     exception-raising accessor. It expects Values.get_exn to report that the
     required question input field is absent. *)
  Alcotest.check_raises "missing input"
    (Dspy_error.Error "missing input field \"question\"")
    (fun () -> ignore (Values.get_exn QA.question Values.empty))

let test_values_validate_rejects_missing_required_input () =
  (* This test validates an empty Values map against the QA signature, whose
     question input is required. It expects validation to fail with the missing
     question input error before any prediction work can run. *)
  Alcotest.check_raises "missing required input"
    (Dspy_error.Error "missing input field \"question\"")
    (fun () -> Values.validate QA.signature Values.empty)

let test_values_validate_rejects_input_from_another_signature () =
  (* This test stores a value under QA.question and then validates it against the
     separate IntQA signature. It verifies that validation checks field identity,
     not only the field name, and rejects a value that does not belong to the
     target signature. *)
  Alcotest.check_raises "wrong signature input"
    (Dspy_error.Error "value for field \"question\" is not an input of this signature")
    (fun () ->
      let values = Values.empty |> Values.set QA.question "What is 2 + 2?" in
      Values.validate IntQA.signature values)

let test_prediction_get_rejects_missing_output () =
  (* This test reads the QA answer output from an empty Prediction map. It
     expects Prediction.get to raise the missing output error for answer rather
     than returning a default or optional value. *)
  Alcotest.check_raises "missing output"
    (Dspy_error.Error "missing output field \"answer\"")
    (fun () -> ignore (Prediction.get QA.answer Prediction.empty))

let test_type_parse_exn_rejects_bad_int () =
  (* This test parses non-numeric text through the integer type parser for the
     answer field. It expects Type.parse_exn to reject the value with an error
     that names the field, the int type, and the expected integer format. *)
  Alcotest.check_raises "bad int"
    (Dspy_error.Error "could not parse field \"answer\" as int: expected integer")
    (fun () -> ignore (Type.parse_exn ~field:"answer" Type.int "not an integer"))

let test_type_parse_exn_rejects_bad_float () =
  (* This test parses non-numeric text through the float type parser for a score
     field. It verifies that Type.parse_exn raises the field-specific float
     parse error instead of accepting arbitrary strings. *)
  Alcotest.check_raises "bad float"
    (Dspy_error.Error "could not parse field \"score\" as float: expected float")
    (fun () -> ignore (Type.parse_exn ~field:"score" Type.float "not a float"))

let test_type_parse_exn_rejects_bad_bool () =
  (* This test parses the string "maybe" through the boolean type parser. It
     expects Type.parse_exn to accept only supported boolean spellings and to
     report that the flag field must be true or false. *)
  Alcotest.check_raises "bad bool"
    (Dspy_error.Error "could not parse field \"flag\" as bool: expected true or false")
    (fun () -> ignore (Type.parse_exn ~field:"flag" Type.bool "maybe"))

let test_type_parse_exn_rejects_bad_json () =
  (* This test parses invalid JSON text through the JSON type parser for the
     payload field. It expects the parser to surface a Dspy_error containing the
     field name, json type, and underlying JSON lexer error. *)
  Alcotest.check_raises "bad json"
    (Dspy_error.Error
       "could not parse field \"payload\" as json: Line 1, bytes 0-8:\nInvalid token 'not-json'")
    (fun () -> ignore (Type.parse_exn ~field:"payload" Type.json "not-json"))

let test_type_parse_exn_rejects_non_array_list () =
  (* This test parses a valid JSON scalar using the list-of-int type parser. It
     verifies that list parsing requires the top-level JSON value to be an array
     and reports the field-specific expected-array error for items. *)
  Alcotest.check_raises "non-array list"
    (Dspy_error.Error "could not parse field \"items\" as list(int): expected JSON array")
    (fun () -> ignore (Type.parse_exn ~field:"items" (Type.list Type.int) "1"))

let test_type_parse_exn_rejects_malformed_json_list () =
  (* This test parses malformed JSON text using the list-of-int type parser. It
     expects Type.parse_exn to fail before item decoding and wrap the JSON lexer
     failure in an error that identifies the items list field. *)
  Alcotest.check_raises "malformed JSON list"
    (Dspy_error.Error
       "could not parse field \"items\" as list(int): Line 1, bytes 0-8:\nInvalid token 'not-json'")
    (fun () -> ignore (Type.parse_exn ~field:"items" (Type.list Type.int) "not-json"))

let test_type_parse_exn_rejects_bad_list_item () =
  (* This test parses a JSON array whose element is a string while the expected
     element type is int. It verifies that list item decoding fails with an error
     tied to the items field and explains that the array element is not a JSON
     integer. *)
  Alcotest.check_raises "bad list item"
    (Dspy_error.Error
       "could not parse field \"items\" as list(int): expected JSON integer, got \"no\"")
    (fun () -> ignore (Type.parse_exn ~field:"items" (Type.list Type.int) "[\"no\"]"))

let test_chat_adapter_render_rejects_missing_values () =
  (* This test renders the QA signature through the default chat adapter with no
     input values supplied. It expects rendering to validate required inputs and
     fail with the missing question field error before producing LM messages. *)
  Alcotest.check_raises "adapter render missing values"
    (Dspy_error.Error "missing input field \"question\"")
    (fun () -> ignore (Chat_adapter.render Chat_adapter.default QA.signature Values.empty))

let test_chat_adapter_parse_rejects_missing_marker () =
  (* This test parses a plain text LM response that contains no DSPy output
     marker. It expects the chat adapter to reject the response because the
     required answer marker is missing. *)
  Alcotest.check_raises "missing output marker"
    (Dspy_error.Error "LM response missing output marker for field \"answer\"")
    (fun () -> ignore (Chat_adapter.parse Chat_adapter.default QA.signature "Paris"))

let test_chat_adapter_parse_rejects_duplicate_marker () =
  (* This test parses an LM response that repeats the answer output marker. It
     verifies that the adapter treats duplicate markers as ambiguous output and
     raises the duplicate-marker error for the answer field. *)
  Alcotest.check_raises "duplicate output marker"
    (Dspy_error.Error "LM response contains duplicate output marker for field \"answer\"")
    (fun () ->
      ignore
        (Chat_adapter.parse Chat_adapter.default QA.signature
           "[[ ## answer ## ]]\nParis\n[[ ## answer ## ]]\nLyon"))

let test_chat_adapter_parse_rejects_unknown_marker () =
  (* This test parses an LM response that includes an undeclared confidence
     marker before the valid answer marker. It expects the adapter to reject
     unknown output markers instead of silently ignoring extra structured
     fields. *)
  Alcotest.check_raises "unknown output marker"
    (Dspy_error.Error "LM response contains unknown output marker \"confidence\"")
    (fun () ->
      ignore
        (Chat_adapter.parse Chat_adapter.default QA.signature
           "[[ ## confidence ## ]]\n0.9\n[[ ## answer ## ]]\nParis"))

let test_chat_adapter_parse_rejects_bad_typed_field () =
  (* This test parses a response for the integer-answer signature where the
     answer marker contains non-integer text. It expects marker extraction to
     succeed but typed output parsing to fail with the answer int parse error. *)
  Alcotest.check_raises "bad typed output"
    (Dspy_error.Error "could not parse field \"answer\" as int: expected integer")
    (fun () ->
      ignore
        (Chat_adapter.parse Chat_adapter.default IntQA.signature
           "[[ ## answer ## ]]\nnot an integer"))

let test_predict_forward_rejects_missing_values () =
  (* This test constructs a Predict value around a mock LM that would otherwise
     return a valid answer marker, then calls Predict.forward with an empty
     Values map. The behavior under test is the preflight validation of required
     signature inputs: the predictor must fail before invoking or depending on
     the LM response, and the expected exception names the missing question
     input field. *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let module Unused_lm : Ocaml_dspy.Lm = struct
    let model = "mock"
    let forward ~sw:_ ~env:_ _ = Lm_response.of_text "[[ ## answer ## ]]\nParis"
  end in
  let predictor = Predict.create ~lm:(module Unused_lm) QA.signature in
  Alcotest.check_raises "predict missing values"
    (Dspy_error.Error "missing input field \"question\"")
    (fun () -> ignore (Predict.forward ~sw ~env predictor Values.empty))

let test_predict_forward_propagates_lm_exception () =
  (* This test uses a complete QA input set with a mock LM whose forward
     function raises Failure "mock LM failed". It verifies that Predict.forward
     does not hide or rewrite unexpected backend failures after input validation
     succeeds; the exact LM exception should propagate to the caller. *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let module Exploding_lm : Ocaml_dspy.Lm = struct
    let model = "mock"
    let forward ~sw:_ ~env:_ _ = failwith "mock LM failed"
  end in
  let predictor = Predict.create ~lm:(module Exploding_lm) QA.signature in
  let values = Values.empty |> Values.set QA.question "What is the capital of France?" in
  Alcotest.check_raises "predict LM exception"
    (Failure "mock LM failed")
    (fun () -> ignore (Predict.forward ~sw ~env predictor values))

let test_predict_forward_rejects_unparseable_lm_response () =
  (* This test builds an integer-answer predictor with a mock LM that returns an
     answer marker containing non-integer text. The setup supplies all required
     inputs so the failure must come from response parsing, and the expected
     exception is the field-specific integer parse error for answer. *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let module Bad_int_lm : Ocaml_dspy.Lm = struct
    let model = "mock"
    let forward ~sw:_ ~env:_ _ = Lm_response.of_text "[[ ## answer ## ]]\nnot an integer"
  end in
  let predictor = Predict.create ~lm:(module Bad_int_lm) IntQA.signature in
  let values = Values.empty |> Values.set IntQA.question "Give a number." in
  Alcotest.check_raises "predict response parse error"
    (Dspy_error.Error "could not parse field \"answer\" as int: expected integer")
    (fun () -> ignore (Predict.forward ~sw ~env predictor values))

module QAPredict = [%dspy.predict (module QA) ~lm:(module struct
  let model = "mock"
  let forward ~sw:_ ~env:_ _ = Lm_response.of_text "No markers here"
end : Ocaml_dspy.Lm)]

let test_ppx_predict_call_rejects_unparseable_lm_response () =
  (* This test calls the PPX-generated Predict module whose embedded mock LM
     returns text without DSPy output markers. It exercises the generated call
     wrapper, including labeled argument packing and prediction decoding, and
     expects the adapter parse failure for the missing answer marker. *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Alcotest.check_raises "generated predict parse error"
    (Dspy_error.Error "LM response missing output marker for field \"answer\"")
    (fun () -> ignore (QAPredict.call ~sw ~env ~question:"What is the capital of France?"))

let test_ppx_signature_decode_rejects_missing_prediction_output () =
  (* This test calls the PPX-generated QA.decode helper with an empty Prediction
     map. It bypasses the LM and adapter layers to check only generated output
     decoding, expecting the decoder to require the answer field and raise the
     missing-output exception when it is absent. *)
  Alcotest.check_raises "generated decode missing output"
    (Dspy_error.Error "missing output field \"answer\"")
    (fun () -> ignore (QA.decode Prediction.empty))

let test_chain_of_thought_create_rejects_existing_reasoning_output () =
  (* This test creates a Chain_of_thought module for a signature that already
     declares reasoning as a normal output. Chain_of_thought adds its own
     reasoning field, so creation should reject the resulting duplicate output
     name before any LM call is possible. *)
  let module Unused_lm : Ocaml_dspy.Lm = struct
    let model = "mock"
    let forward ~sw:_ ~env:_ _ = Lm_response.of_text ""
  end in
  Alcotest.check_raises "chain_of_thought duplicate reasoning"
    (Dspy_error.Error "duplicate field \"reasoning\" in signature (output and output)")
    (fun () ->
      ignore
        (Chain_of_thought.create ~lm:(module Unused_lm) ReasoningCollision.signature))

let test_chain_of_thought_forward_rejects_missing_reasoning () =
  (* This test runs a Chain_of_thought predictor with valid QA input but a mock
     LM response that contains only the final answer marker. It verifies that
     chain-of-thought parsing requires the generated reasoning output as well as
     the original signature outputs, raising the missing reasoning marker error. *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let module Answer_only_lm : Ocaml_dspy.Lm = struct
    let model = "mock"
    let forward ~sw:_ ~env:_ _ = Lm_response.of_text "[[ ## answer ## ]]\nParis"
  end in
  let cot = Chain_of_thought.create ~lm:(module Answer_only_lm) QA.signature in
  let values = Values.empty |> Values.set QA.question "What is the capital of France?" in
  Alcotest.check_raises "chain_of_thought missing reasoning"
    (Dspy_error.Error "LM response missing output marker for field \"reasoning\"")
    (fun () -> ignore (Chain_of_thought.forward ~sw ~env cot values))

module QACoT = [%dspy.chain_of_thought (module QA) ~lm:(module struct
  let model = "mock"
  let forward ~sw:_ ~env:_ _ = Lm_response.of_text "[[ ## answer ## ]]\nParis"
end : Ocaml_dspy.Lm)]

let test_ppx_chain_of_thought_call_rejects_missing_reasoning () =
  (* This test exercises the PPX-generated Chain_of_thought call wrapper whose
     embedded mock LM returns only an answer marker. The labeled input argument
     is valid, so the expected failure is produced while decoding the generated
     reasoning-plus-output response shape, specifically the missing reasoning
     marker exception. *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Alcotest.check_raises "generated chain_of_thought missing reasoning"
    (Dspy_error.Error "LM response missing output marker for field \"reasoning\"")
    (fun () -> ignore (QACoT.call ~sw ~env ~question:"What is the capital of France?"))

let test_openai_response_of_string_rejects_invalid_json () =
  (* This test feeds non-JSON text to the OpenAI-compatible response string
     decoder. It checks the string parsing boundary before any chat-response
     shape validation, expecting the invalid JSON lexer failure to be wrapped in
     the library's Dspy_error message. *)
  Alcotest.check_raises "invalid OpenAI JSON"
    (Dspy_error.Error
       "OpenAI-compatible chat response was not valid JSON: Line 1, bytes 0-8:\nInvalid token 'not-json'")
    (fun () -> ignore (Openai_chat_lm.response_of_string "not-json"))

let test_openai_response_of_yojson_rejects_missing_choices () =
  (* This test passes a JSON object with no choices member to the
     OpenAI-compatible response decoder. It verifies that response shape
     validation requires a choices array and reports the missing-array error
     before trying to read messages or usage. *)
  Alcotest.check_raises "missing OpenAI choices"
    (Dspy_error.Error "OpenAI response missing choices array")
    (fun () -> ignore (Openai_chat_lm.response_of_yojson (`Assoc [])))

let test_openai_response_of_yojson_rejects_empty_choices () =
  (* This test passes an OpenAI-style response object whose choices array exists
     but is empty. The expected behavior is to reject the response as unusable,
     since there is no assistant message to convert into an Lm_response. *)
  Alcotest.check_raises "empty OpenAI choices"
    (Dspy_error.Error "OpenAI response choices array is empty")
    (fun () ->
      ignore (Openai_chat_lm.response_of_yojson (`Assoc [ ("choices", `List []) ])))

let test_openai_response_of_yojson_rejects_choice_without_message () =
  (* This test supplies a choices array containing an object with a finish reason
     but no message object. It verifies that the OpenAI response decoder requires
     the assistant message payload for the first choice and raises the specific
     missing-message exception. *)
  Alcotest.check_raises "OpenAI choice without message"
    (Dspy_error.Error "OpenAI response choice missing message object")
    (fun () ->
      ignore
        (Openai_chat_lm.response_of_yojson
           (`Assoc [ ("choices", `List [ `Assoc [ ("finish_reason", `String "stop") ] ]) ])))

let test_openai_response_of_yojson_rejects_message_without_text () =
  (* This test supplies a choice whose message object contains only a role and no
     textual content. It checks that conversion to Lm_response refuses messages
     without text content and reports the missing-text error rather than
     returning an empty response. *)
  Alcotest.check_raises "OpenAI message without text"
    (Dspy_error.Error "OpenAI response message missing text content")
    (fun () ->
      ignore
        (Openai_chat_lm.response_of_yojson
           (`Assoc
             [
               ( "choices",
                 `List
                   [
                     `Assoc
                       [ ("message", `Assoc [ ("role", `String "assistant") ]) ];
                   ] );
             ])))

let test_openai_response_of_yojson_rejects_non_object_usage () =
  (* This test decodes an otherwise usable OpenAI-style response that includes a
     usage field with the wrong JSON shape. It expects the decoder to reject
     non-object usage metadata explicitly instead of ignoring malformed token
     accounting data. *)
  Alcotest.check_raises "OpenAI non-object usage"
    (Dspy_error.Error "OpenAI response usage must be an object")
    (fun () ->
      ignore
        (Openai_chat_lm.response_of_yojson
           (`Assoc
             [
               ( "choices",
                 `List
                   [
                     `Assoc
                       [
                         ( "message",
                           `Assoc
                             [
                               ("role", `String "assistant");
                               ("content", `String "hello");
                             ] );
                       ];
                   ] );
               ("usage", `String "not an object");
             ])))

let () =
  Alcotest.run "ocaml_dspy_exceptions"
    [
      ( "signature",
        [
          Alcotest.test_case "input rejects empty names" `Quick
            test_signature_input_rejects_empty_name;
          Alcotest.test_case "output rejects empty names" `Quick
            test_signature_output_rejects_empty_name;
          Alcotest.test_case "make rejects duplicate inputs" `Quick
            test_signature_make_rejects_duplicate_inputs;
          Alcotest.test_case "make rejects duplicate outputs" `Quick
            test_signature_make_rejects_duplicate_outputs;
          Alcotest.test_case "make rejects duplicate input/output names" `Quick
            test_signature_make_rejects_duplicate_input_output;
        ] );
      ( "values-and-predictions",
        [
          Alcotest.test_case "get_exn rejects missing inputs" `Quick
            test_values_get_exn_rejects_missing_input;
          Alcotest.test_case "validate rejects missing required inputs" `Quick
            test_values_validate_rejects_missing_required_input;
          Alcotest.test_case "validate rejects values from another signature" `Quick
            test_values_validate_rejects_input_from_another_signature;
          Alcotest.test_case "Prediction.get rejects missing outputs" `Quick
            test_prediction_get_rejects_missing_output;
        ] );
      ( "types",
        [
          Alcotest.test_case "parse_exn rejects invalid ints" `Quick
            test_type_parse_exn_rejects_bad_int;
          Alcotest.test_case "parse_exn rejects invalid floats" `Quick
            test_type_parse_exn_rejects_bad_float;
          Alcotest.test_case "parse_exn rejects invalid bools" `Quick
            test_type_parse_exn_rejects_bad_bool;
          Alcotest.test_case "parse_exn rejects invalid JSON" `Quick
            test_type_parse_exn_rejects_bad_json;
          Alcotest.test_case "parse_exn rejects non-array lists" `Quick
            test_type_parse_exn_rejects_non_array_list;
          Alcotest.test_case "parse_exn rejects malformed JSON lists" `Quick
            test_type_parse_exn_rejects_malformed_json_list;
          Alcotest.test_case "parse_exn rejects invalid list items" `Quick
            test_type_parse_exn_rejects_bad_list_item;
        ] );
      ( "chat-adapter",
        [
          Alcotest.test_case "render rejects missing values" `Quick
            test_chat_adapter_render_rejects_missing_values;
          Alcotest.test_case "parse rejects missing markers" `Quick
            test_chat_adapter_parse_rejects_missing_marker;
          Alcotest.test_case "parse rejects duplicate markers" `Quick
            test_chat_adapter_parse_rejects_duplicate_marker;
          Alcotest.test_case "parse rejects unknown markers" `Quick
            test_chat_adapter_parse_rejects_unknown_marker;
          Alcotest.test_case "parse rejects unparseable typed fields" `Quick
            test_chat_adapter_parse_rejects_bad_typed_field;
        ] );
      ( "predict",
        [
          Alcotest.test_case "forward rejects missing values" `Quick
            test_predict_forward_rejects_missing_values;
          Alcotest.test_case "forward propagates LM exceptions" `Quick
            test_predict_forward_propagates_lm_exception;
          Alcotest.test_case "forward rejects unparseable LM responses" `Quick
            test_predict_forward_rejects_unparseable_lm_response;
          Alcotest.test_case "generated call rejects unparseable LM responses" `Quick
            test_ppx_predict_call_rejects_unparseable_lm_response;
          Alcotest.test_case "generated decode rejects missing prediction outputs" `Quick
            test_ppx_signature_decode_rejects_missing_prediction_output;
        ] );
      ( "chain-of-thought",
        [
          Alcotest.test_case "create rejects an existing reasoning output" `Quick
            test_chain_of_thought_create_rejects_existing_reasoning_output;
          Alcotest.test_case "forward rejects missing reasoning" `Quick
            test_chain_of_thought_forward_rejects_missing_reasoning;
          Alcotest.test_case "generated call rejects missing reasoning" `Quick
            test_ppx_chain_of_thought_call_rejects_missing_reasoning;
        ] );
      ( "openai-json",
        [
          Alcotest.test_case "response_of_string rejects invalid JSON" `Quick
            test_openai_response_of_string_rejects_invalid_json;
          Alcotest.test_case "response_of_yojson rejects missing choices" `Quick
            test_openai_response_of_yojson_rejects_missing_choices;
          Alcotest.test_case "response_of_yojson rejects empty choices" `Quick
            test_openai_response_of_yojson_rejects_empty_choices;
          Alcotest.test_case "response_of_yojson rejects choices without messages" `Quick
            test_openai_response_of_yojson_rejects_choice_without_message;
          Alcotest.test_case "response_of_yojson rejects messages without text" `Quick
            test_openai_response_of_yojson_rejects_message_without_text;
          Alcotest.test_case "response_of_yojson rejects non-object usage" `Quick
            test_openai_response_of_yojson_rejects_non_object_usage;
        ] );
    ]
