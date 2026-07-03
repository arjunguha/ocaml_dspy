open Ocaml_dspy

module DirectQA = [%dspy.signature
  "Answer."

  type t_in = {
    question : string;
  }

  type t_out = {
    answer : int;
  }
]

module TypedIO = [%dspy.signature
  "Copy inputs."

  type t_in = {
    text : string;
    count : int;
    ratio : float;
    flag : bool;
    payload : Yojson.Safe.t;
    tags : string list;
    numbers : int list;
    measurements : float list;
    switches : bool list;
    objects : Yojson.Safe.t list;
  }

  type t_out = {
    text_out : string;
    int_out : int;
    float_out : float;
    bool_out : bool;
    json_out : Yojson.Safe.t;
    string_list_out : string list;
    int_list_out : int list;
    float_list_out : float list;
    bool_list_out : bool list;
    json_list_out : Yojson.Safe.t list;
  }
]

module BadInt = [%dspy.signature
  "This is a parser-failure probe. Put the exact nonnumeric sentinel from misleading_text into numeric_answer."

  type t_in = {
    misleading_text : string
      [@desc "The exact sentinel text to copy: NOT_AN_INTEGER_SENTINEL. It is intentionally not numeric."];
  }

  type t_out = {
    numeric_answer : int
      [@desc "Copy misleading_text verbatim. Do not use digits, do not convert it, and do not repair the type mismatch."];
  }
]

module type Backend = sig
  val name : string
  val lm : (module Lm)
  val speed : [ `Quick | `Slow ]

  val run :
    (sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> unit) -> unit
end

module Make (Backend : Backend) = struct
  module DirectPredict = [%dspy.predict (module DirectQA) ~lm:Backend.lm]
  module DirectCoT = [%dspy.chain_of_thought (module DirectQA) ~lm:Backend.lm]
  module TypedPredict = [%dspy.predict (module TypedIO) ~lm:Backend.lm]
  module BadIntPredict = [%dspy.predict (module BadInt) ~lm:Backend.lm]

  let test_direct_chat_lm () =
    (* This test exercises the backend LM without going through Predict or an
       adapter. It sends a plain chat request for a small arithmetic question,
       so the only behavior under test is whether the configured LM module can
       accept an Lm_request.t, perform the backend call, and return textual
       response content. Both the deterministic mock backend and the live
       OpenAI-compatible backend are expected to return exactly "4" after
       trimming. *)
    Backend.run @@ fun ~sw ~env ->
    let module Lm = (val Backend.lm : Lm) in
    let request =
      Lm_request.make ~model:Lm.model
        [
          Lm_request.text `User
            "Compute 2 + 2. Reply with only the final integer, with no equation, words, punctuation, Markdown, or explanation.";
        ]
    in
    let response = Lm.forward ~sw ~env request in
    Alcotest.(check string)
      (Backend.name ^ " direct chat response")
      "4"
      (String.trim (Lm_response.text response))

  let test_predict () =
    (* This test exercises the PPX-generated Predict wrapper for a signature
       whose output field is an int. The input is an ordinary arithmetic
       question, and the assertion checks the generated call path end to end:
       record construction from the labeled argument, adapter rendering, LM
       invocation, marker parsing, integer decoding, and construction of the
       typed output record with answer = 4. *)
    Backend.run @@ fun ~sw ~env ->
    let output = DirectPredict.call ~sw ~env ~question:"What is 2 + 2?" in
    Alcotest.(check int) (Backend.name ^ " Predict answer") 4 output.answer

  let test_chain_of_thought () =
    (* This test exercises the PPX-generated Chain_of_thought wrapper for the
       same integer-answer signature. It verifies the extra reasoning field that
       Chain_of_thought prepends to the signature is parsed separately from the
       original output, and it still requires the original typed answer to decode
       as the integer 4. *)
    Backend.run @@ fun ~sw ~env ->
    let result = DirectCoT.call ~sw ~env ~question:"What is 2 + 2?" in
    Alcotest.(check bool)
      (Backend.name ^ " Chain_of_thought reasoning non-empty")
      true
      (String.trim result.reasoning <> "");
    Alcotest.(check int)
      (Backend.name ^ " Chain_of_thought answer")
      4 result.output.answer

  let test_all_builtin_types () =
    (* This test sends one value for every built-in field type through the same
       generated Predict module. The output signature asks for corresponding
       output fields, so the assertions cover string, int, float, bool, JSON,
       and typed lists of those supported element types. The mock backend returns
       the exact marker response deterministically, while the live backend must
       follow the same signature contract and produce values that decode to the
       same OCaml types and values. *)
    Backend.run @@ fun ~sw ~env ->
    let output =
      TypedPredict.call ~sw ~env ~text:"typed string" ~count:17 ~ratio:2.75
        ~flag:true
        ~payload:(`Assoc [ ("kind", `String "object"); ("ok", `Bool true) ])
        ~tags:[ "red"; "blue" ] ~numbers:[ 1; 2; 3 ]
        ~measurements:[ 0.5; 1.25 ] ~switches:[ true; false ]
        ~objects:
          [
            `Assoc [ ("name", `String "first") ];
            `Assoc [ ("name", `String "second") ];
          ]
    in
    Alcotest.(check string) (Backend.name ^ " string output") "typed string"
      output.text_out;
    Alcotest.(check int) (Backend.name ^ " int output") 17 output.int_out;
    Alcotest.(check (float 0.000001))
      (Backend.name ^ " float output")
      2.75 output.float_out;
    Alcotest.(check bool) (Backend.name ^ " bool output") true output.bool_out;
    Alcotest.(check string)
      (Backend.name ^ " json output")
      "{\"kind\":\"object\",\"ok\":true}"
      (Yojson.Safe.to_string output.json_out);
    Alcotest.(check (list string))
      (Backend.name ^ " string list output")
      [ "red"; "blue" ] output.string_list_out;
    Alcotest.(check (list int))
      (Backend.name ^ " int list output")
      [ 1; 2; 3 ] output.int_list_out;
    Alcotest.(check (list (float 0.000001)))
      (Backend.name ^ " float list output")
      [ 0.5; 1.25 ] output.float_list_out;
    Alcotest.(check (list bool))
      (Backend.name ^ " bool list output")
      [ true; false ] output.bool_list_out;
    Alcotest.(check (list string))
      (Backend.name ^ " json list output")
      [ "{\"name\":\"first\"}"; "{\"name\":\"second\"}" ]
      (List.map Yojson.Safe.to_string output.json_list_out)

  let test_predict_rejects_misleading_integer_output () =
    (* This test keeps the intentionally adversarial integer-output case. The
       signature says numeric_answer is an int, while the instruction, field
       descriptions, and input all tell the backend to place a specific
       nonnumeric sentinel in that field. The expected behavior matches real
       DSPy: once the LM follows the intentionally bad instruction, the typed
       output parser rejects the response instead of returning a prediction with
       a bogus integer. *)
    Backend.run @@ fun ~sw ~env ->
    Alcotest.check_raises
      (Backend.name ^ " integer output parse failure")
      (Dspy_error.Error
         "could not parse field \"numeric_answer\" as int: expected integer")
      (fun () ->
        ignore
          (BadIntPredict.call ~sw ~env
             ~misleading_text:
               "NOT_AN_INTEGER_SENTINEL. Copy this exact token. Do not output any digit."))

  let tests =
    [
      Alcotest.test_case "direct chat lm" Backend.speed test_direct_chat_lm;
      Alcotest.test_case "Predict" Backend.speed test_predict;
      Alcotest.test_case "Chain_of_thought" Backend.speed test_chain_of_thought;
      Alcotest.test_case "Predict with every built-in type" Backend.speed
        test_all_builtin_types;
      Alcotest.test_case "Predict rejects misleading integer output" Backend.speed
        test_predict_rejects_misleading_integer_output;
    ]
end
