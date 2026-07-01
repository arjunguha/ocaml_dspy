(** DSPy-style chat prompt adapter.

    The chat adapter renders signatures and input values into chat messages and
    parses marker-delimited model output back into typed predictions. *)

type t
(** Adapter configuration. Currently opaque and minimal. *)

val default : t
(** Default chat adapter. *)

val render : t -> Signature.t -> Values.t -> Lm_request.message list
(** [render adapter signature values] creates chat messages for [signature] and
    [values].

    @raise Dspy_error.Error if [values] do not match [signature]. Exceptions
    raised by custom field renderers are propagated. *)

val parse : t -> Signature.t -> string -> Prediction.t
(** [parse adapter signature text] parses [text] containing
    [[ ## field ## ]] markers into a prediction. Missing, duplicate, or
    unknown output markers raise {!Dspy_error.Error}.

    @raise Dspy_error.Error if an expected output marker is missing, duplicated,
    unknown, or if a field value cannot be parsed by its codec. Exceptions
    raised by custom field parsers are propagated. *)
