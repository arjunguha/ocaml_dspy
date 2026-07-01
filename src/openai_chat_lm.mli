(** OpenAI-compatible Chat Completions LM.

    The implementation reads [OPENAI_API_BASE] and [OPENAI_API_KEY]. It speaks
    the Chat Completions JSON format and works with LiteLLM-compatible servers.
    The module's default {!val:model} is ["gpt-5-nano"]; individual requests may
    override it with [Lm_request.make ~model].

    The included [forward] implementation raises {!Dspy_error.Error} for invalid
    TLS setup, malformed HTTPS hosts, non-success HTTP responses, invalid JSON,
    or invalid Chat Completions response shapes. Eio, Cohttp, TLS, DNS, and
    network exceptions may also be propagated. *)

include Lm.S
(** @inline *)

val request_to_yojson : model:string -> Lm_request.t -> Yojson.Safe.t
(** [request_to_yojson ~model request] converts [request] to Chat Completions
    JSON, using [model] when [request.model] is absent. *)

val response_of_yojson : Yojson.Safe.t -> Lm_response.t
(** [response_of_yojson json] decodes Chat Completions response JSON.

    @raise Dspy_error.Error if [json] is not a valid Chat Completions response
    shape. *)

val response_of_string : string -> Lm_response.t
(** [response_of_string text] parses and decodes Chat Completions response JSON.
    Invalid JSON raises {!Dspy_error.Error}.

    @raise Dspy_error.Error if [text] is invalid JSON or does not decode to a
    valid Chat Completions response shape. *)
