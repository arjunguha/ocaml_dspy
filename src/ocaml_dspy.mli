(** Minimal OCaml DSPy port.

    This library exposes a small typed core for signatures, predictions,
    chat-adapter prompting, OpenAI-compatible LMs, prediction, and
    chain-of-thought. *)

module Dspy_error = Dspy_error
(** Error helpers. *)

module Type = Type
(** Typed value codecs. *)

module Signature = Signature
(** Runtime DSPy signatures. *)

module Values = Values
(** Typed input value maps. *)

module Prediction = Prediction
(** Typed output prediction maps. *)

module Lm_request = Lm_request
(** Provider-independent LM requests. *)

module Lm_response = Lm_response
(** Provider-independent LM responses. *)

module Lm = Lm
(** LM provider module type. *)

module Openai_chat_lm = Openai_chat_lm
(** OpenAI-compatible Chat Completions LM. *)

module Chat_adapter = Chat_adapter
(** DSPy-style chat prompt adapter. *)

module Predict = Predict
(** One-shot prediction. *)

module Chain_of_thought = Chain_of_thought
(** Chain-of-thought prediction. *)

module type Lm = Lm.S
(** Alias for the language-model module type. *)
