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

## Installation

Install from the Git repository with opam:

```sh
opam pin add ocaml_dspy git+https://github.com/arjunguha/ocaml_dspy.git
opam pin add ppx_ocaml_dspy git+https://github.com/arjunguha/ocaml_dspy.git
```

## OpenAI-Compatible LM

`Ocaml_dspy.Openai_chat_lm` reads:

```sh
export OPENAI_API_BASE="http://localhost:4000/v1"
export OPENAI_API_KEY="..."
```

`OPENAI_API_BASE` defaults to `http://localhost:4000/v1`, which is convenient
for a local LiteLLM gateway. The default model is `gpt-5-nano`.

## Example

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
