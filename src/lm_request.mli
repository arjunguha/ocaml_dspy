(** Provider-independent language-model request data. *)

type role = [ `System | `User | `Assistant ]
(** Chat message roles supported by the minimal port. *)

type part = Text of string
(** A message part. Only text parts are supported for now. *)

type message = {
  role : role;  (** Role of the chat message. *)
  parts : part list;  (** Ordered message content parts. *)
}
(** A chat message. *)

type config = {
  temperature : float option;  (** Optional sampling temperature. *)
  max_tokens : int option;  (** Optional maximum completion token count. *)
  top_p : float option;  (** Optional nucleus-sampling cutoff. *)
  stop : string list;  (** Stop sequences. *)
}
(** Common generation parameters shared across providers. *)

type t = {
  model : string option;  (** Optional request-specific model override. *)
  messages : message list;  (** Chat messages sent to the model. *)
  config : config;  (** Generation configuration. *)
  metadata : Yojson.Safe.t option;  (** Provider-specific extension data. *)
}
(** A complete language-model request. *)

val default_config : config
(** Default generation config with all optional parameters unset. *)

val text : role -> string -> message
(** [text role content] creates a single-text-part message. *)

val make : ?model:string -> ?config:config -> ?metadata:Yojson.Safe.t -> message list -> t
(** [make ?model ?config ?metadata messages] creates a request. *)

val message_text : message -> string
(** [message_text message] concatenates all text parts in [message]. *)
