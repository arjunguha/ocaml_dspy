open Ppxlib

let assert_signature_expansion_error ~label ~source ~message =
  match
    let lexbuf = Lexing.from_string source in
    let structure = Parse.implementation lexbuf in
    ignore (Ppx_ocaml_dspy.map_structure structure)
  with
  | () -> Alcotest.fail (label ^ " did not fail during PPX expansion")
  | exception Location.Error error ->
      Alcotest.(check string) label message (Location.Error.message error)

let test_signature_rejects_duplicate_input_fields () =
  (* This test expands a signature extension whose input record declares the
     same question field twice with different types. It expects PPX expansion to
     fail before runtime code is produced, reporting the duplicate as an
     input/input field collision. *)
  assert_signature_expansion_error ~label:"duplicate input fields"
    ~message:"duplicate DSPy field \"question\" in signature (input and input)"
    ~source:
      {|
module Bad = [%dspy.signature
  "Bad signature."

  type t_in = {
    question : string;
    question : int;
  }

  type t_out = {
    answer : string;
  }
]
|}

let test_signature_rejects_duplicate_output_fields () =
  (* This test expands a signature extension whose output record repeats the
     answer field. It verifies that the PPX rejects the duplicate output during
     expansion and reports the exact output/output collision message. *)
  assert_signature_expansion_error ~label:"duplicate output fields"
    ~message:"duplicate DSPy field \"answer\" in signature (output and output)"
    ~source:
      {|
module Bad = [%dspy.signature
  "Bad signature."

  type t_in = {
    question : string;
  }

  type t_out = {
    answer : string;
    answer : int;
  }
]
|}

let test_signature_rejects_duplicate_input_output_fields () =
  (* This test expands a signature extension where the input and output records
     both use the answer field name. It expects the PPX duplicate-name check to
     reject the signature and identify the conflict as crossing input and output
     fields. *)
  assert_signature_expansion_error ~label:"duplicate input and output fields"
    ~message:"duplicate DSPy field \"answer\" in signature (input and output)"
    ~source:
      {|
module Bad = [%dspy.signature
  "Bad signature."

  type t_in = {
    answer : string;
  }

  type t_out = {
    answer : string;
  }
]
|}

let () =
  Alcotest.run "ppx_ocaml_dspy_errors"
    [
      ( "signature",
        [
          Alcotest.test_case "rejects duplicate input fields" `Quick
            test_signature_rejects_duplicate_input_fields;
          Alcotest.test_case "rejects duplicate output fields" `Quick
            test_signature_rejects_duplicate_output_fields;
          Alcotest.test_case "rejects duplicate input/output fields" `Quick
            test_signature_rejects_duplicate_input_output_fields;
        ] );
    ]
