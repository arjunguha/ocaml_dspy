# OCaml DSPy

OCaml DSPy is a small OCaml 5 port of the core DSPy programming model. It
provides typed signatures, typed input and output maps, a marker-based chat
adapter, `Predict`, `Chain_of_thought`, and an OpenAI-compatible chat LM that
works with LiteLLM-style servers.

This is intentionally a focused subset. It does not include DSPy optimizers,
history, tools/function calling, streaming, multimodal fields, persistence, or
Python-compatible dynamic signature parsing.

## Packages

The repository defines two Dune packages:

- `ocaml_dspy`: runtime library.
- `ppx_ocaml_dspy`: PPX rewriter for ergonomic signature and predictor modules.

Runtime dependencies include `eio`, `cohttp-eio`, `tls-eio`, `ca-certs`,
`yojson`, `hmap`, `uri`, and `mirage-crypto-rng`.

## OpenAI-Compatible LM

`Ocaml_dspy.Openai_chat_lm` reads:

```sh
export OPENAI_API_BASE="http://localhost:4000/v1"
export OPENAI_API_KEY="..."
```

`OPENAI_API_BASE` defaults to `http://localhost:4000/v1`, which is convenient
for a local LiteLLM gateway. The default model is `gpt-5-nano`.

## PPX Example

```ocaml
open Ocaml_dspy

let lm = (module Openai_chat_lm : Lm)

module QA = [%dspy.signature
  "Answer questions concisely."

  type t_in = {
    question : string [@desc "Question to answer"];
  }

  type t_out = {
    answer : string [@desc "Concise answer"];
  }
]

module QAPredict = [%dspy.predict (module QA) ~lm]

let main ~sw ~env =
  let output =
    QAPredict.call ~sw ~env
      ~question:"What is the capital of France?"
  in
  print_endline output.answer

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  main ~sw ~env
```

Dune stanza:

```lisp
(executable
 (name app)
 (libraries ocaml_dspy eio_main)
 (preprocess
  (pps ppx_ocaml_dspy)))
```

## Non-PPX Example

```ocaml
open Ocaml_dspy

let question =
  Signature.input ~desc:"Question to answer" "question" Type.string

let answer =
  Signature.output ~desc:"Concise answer" "answer" Type.string

let signature =
  Signature.make
    ~instructions:"Answer questions concisely."
    ~inputs:[ Signature.Packed_input question ]
    ~outputs:[ Signature.Packed_output answer ]
    ()

let lm = (module Openai_chat_lm : Lm)

let main ~sw ~env =
  let predictor = Predict.create ~lm signature in
  let inputs =
    Values.empty
    |> Values.set question "What is the capital of France?"
  in
  let prediction = Predict.forward ~sw ~env predictor inputs in
  print_endline (Prediction.get answer prediction)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  main ~sw ~env
```

## Chain Of Thought

`Chain_of_thought` prepends a `reasoning : string` output field and returns the
reasoning separately from the original prediction.

```ocaml
module QACoT = [%dspy.chain_of_thought (module QA) ~lm]

let main ~sw ~env =
  let result =
    QACoT.call ~sw ~env
      ~question:"What is the capital of France?"
  in
  Printf.printf "Reasoning: %s\nAnswer: %s\n"
    result.reasoning result.output.answer
```

## Development

Build everything:

```sh
dune build
```

Run deterministic tests:

```sh
dune exec test/test_mock_ocaml_dspy.exe
dune exec test/test_core_ocaml_dspy.exe
dune exec test/test_exceptions_ocaml_dspy.exe
dune exec test/test_ppx_ocaml_dspy_errors.exe
```

Run live OpenAI-compatible tests:

```sh
OPENAI_API_BASE="http://localhost:4000/v1" \
OPENAI_API_KEY="..." \
dune exec test/test_live_openai_chat_lm.exe
```

Build documentation:

```sh
dune build @doc
```

The generated HTML is under `_build/default/_doc/_html`.
