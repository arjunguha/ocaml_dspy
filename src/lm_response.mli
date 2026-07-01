(** Provider-independent language-model response data. *)

type text_part = { text : string }
(** A generated text part. *)

type output = {
  role : string option;  (** Optional provider role, usually ["assistant"]. *)
  parts : text_part list;  (** Ordered output content parts. *)
}
(** One generated output candidate. *)

type usage = {
  prompt_tokens : int option;  (** Prompt token count, when reported. *)
  completion_tokens : int option;  (** Completion token count, when reported. *)
  total_tokens : int option;  (** Total token count, when reported. *)
}
(** Token usage metadata. *)

type t = {
  outputs : output list;  (** Generated outputs in provider order. *)
  usage : usage option;  (** Optional token usage. *)
  finish_reason : string option;  (** Provider finish reason, if available. *)
  metadata : Yojson.Safe.t option;  (** Provider-specific raw metadata. *)
}
(** A complete language-model response. *)

val of_text : string -> t
(** [of_text text] creates a response with one assistant text output. *)

val text : t -> string
(** [text response] concatenates all text output parts across outputs. *)
