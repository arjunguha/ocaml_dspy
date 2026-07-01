(** Predict modules call an LM once for a signature. *)

type t
(** A configured predictor: signature, adapter, generation config, and LM. *)

val create :
  ?adapter:Chat_adapter.t ->
  ?config:Lm_request.config ->
  lm:(module Lm.S) ->
  Signature.t ->
  t
(** [create ?adapter ?config ~lm signature] configures a predictor for
    [signature]. *)

val forward :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  t ->
  Values.t ->
  Prediction.t
(** [forward ~sw ~env predictor values] renders [values], calls the LM, and
    parses the response into typed predictions.

    @raise Dspy_error.Error if [values] do not match the configured signature,
    if the LM response cannot be parsed, or if the configured LM reports a
    library-level error. Exceptions raised by custom codecs, the configured LM,
    or the Eio runtime are propagated. *)
