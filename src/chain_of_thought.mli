(** Chain-of-thought prediction.

    This wraps a signature by prepending a [reasoning : string] output field,
    then decodes the original outputs normally. *)

type t
(** A configured chain-of-thought predictor. *)

type result = {
  reasoning : string;  (** The generated reasoning field. *)
  prediction : Prediction.t;  (** Predictions for reasoning plus original outputs. *)
}
(** Result of a chain-of-thought forward pass. *)

val create :
  ?adapter:Chat_adapter.t ->
  ?config:Lm_request.config ->
  lm:(module Lm.S) ->
  Signature.t ->
  t
(** [create ?adapter ?config ~lm signature] configures chain-of-thought for
    [signature].

    @raise Dspy_error.Error if [signature] already contains a field named
    ["reasoning"]. *)

val forward :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  t ->
  Values.t ->
  result
(** [forward ~sw ~env cot values] calls the LM and returns reasoning plus the
    typed prediction map.

    @raise Dspy_error.Error if [values] do not match the configured signature,
    if the LM response cannot be parsed, if the reasoning field is missing, or
    if the configured LM reports a library-level error. Exceptions raised by
    custom codecs, the configured LM, or the Eio runtime are propagated. *)
