(** Language-model provider interface. *)

module type S = sig
  (** A synchronous Eio-backed language model. *)

  val model : string
  (** Default model name used when requests do not override it. *)

  val forward :
    sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    Lm_request.t ->
    Lm_response.t
  (** [forward ~sw ~env request] sends [request] and returns the provider
      response. [sw] and [env] expose Eio resource lifetime and OS capabilities
      explicitly. Implementations may raise provider, transport, parsing, or
      Eio exceptions; see the concrete LM module for its documented exceptions. *)
end
