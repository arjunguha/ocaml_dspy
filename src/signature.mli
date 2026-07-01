(** DSPy-style signatures.

    A signature records task instructions plus ordered typed input and output
    fields. PPX-generated modules wrap this lower-level representation in
    ordinary OCaml records. *)

type field = private {
  name : string;  (** Field name used in prompts and markers. *)
  type_name : string;  (** Human-readable type name from the field codec. *)
  desc : string option;  (** Optional field description for prompt rendering. *)
  prefix : string option;  (** Optional prompt prefix override. *)
}
(** Shared metadata for an input or output field. *)

type 'a input
(** A typed input field witness. Values stored under this field have type ['a]. *)

type 'a output
(** A typed output field witness. Predictions read under this field have type ['a]. *)

type packed_input = Packed_input : 'a input -> packed_input
(** Existential wrapper for ordered heterogeneous input fields. *)

type packed_output = Packed_output : 'a output -> packed_output
(** Existential wrapper for ordered heterogeneous output fields. *)

type t
(** Runtime signature data: instructions, inputs, and outputs. *)

val input : ?desc:string -> ?prefix:string -> string -> 'a Type.t -> 'a input
(** [input ?desc ?prefix name typ] creates an input field named [name].

    @raise Dspy_error.Error if [name] is empty or only whitespace. *)

val output : ?desc:string -> ?prefix:string -> string -> 'a Type.t -> 'a output
(** [output ?desc ?prefix name typ] creates an output field named [name].

    @raise Dspy_error.Error if [name] is empty or only whitespace. *)

val make :
  instructions:string ->
  inputs:packed_input list ->
  outputs:packed_output list ->
  unit ->
  t
(** [make ~instructions ~inputs ~outputs ()] builds a signature.

    Field order is preserved. Duplicate names across inputs and outputs raise
    {!Dspy_error.Error}.

    @raise Dspy_error.Error if any field name appears more than once across
    [inputs] and [outputs]. *)

val instructions : t -> string
(** [instructions t] returns the task instructions. *)

val with_instructions : string -> t -> t
(** [with_instructions instructions t] returns [t] with replaced instructions. *)

val append_instructions : string -> t -> t
(** [append_instructions extra t] appends [extra] to [t]'s instructions. *)

val inputs : t -> packed_input list
(** [inputs t] returns input fields in declaration order. *)

val outputs : t -> packed_output list
(** [outputs t] returns output fields in declaration order. *)

val input_field : 'a input -> field
(** [input_field input] returns metadata for [input]. *)

val output_field : 'a output -> field
(** [output_field output] returns metadata for [output]. *)

val input_type : 'a input -> 'a Type.t
(** [input_type input] returns the codec associated with [input]. *)

val output_type : 'a output -> 'a Type.t
(** [output_type output] returns the codec associated with [output]. *)

val input_name : 'a input -> string
(** [input_name input] is [(input_field input).name]. *)

val output_name : 'a output -> string
(** [output_name output] is [(output_field output).name]. *)

module Private : sig
  (** Internal operations shared by library modules.

      This module is not needed for normal users. It avoids exposing the
      concrete typed-map key representation through the main signature API. *)

  type hidden_key
  (** Type-erased field identity. *)

  type map
  (** Typed heterogeneous storage used by {!Values} and {!Prediction}. *)

  val equal_hidden_key : hidden_key -> hidden_key -> bool
  (** [equal_hidden_key a b] is [true] when [a] and [b] identify the same field. *)

  val input_hidden_key : 'a input -> hidden_key
  (** [input_hidden_key input] returns [input]'s type-erased identity. *)

  val output_hidden_key : 'a output -> hidden_key
  (** [output_hidden_key output] returns [output]'s type-erased identity. *)

  val empty_map : map
  (** Empty typed value map. *)

  val add_input : 'a input -> 'a -> map -> map
  (** [add_input input value map] binds [value] under [input]. *)

  val find_input : 'a input -> map -> 'a option
  (** [find_input input map] returns the value bound under [input], if present. *)

  val mem_input : 'a input -> map -> bool
  (** [mem_input input map] checks whether [input] is bound. *)

  val add_output : 'a output -> 'a -> map -> map
  (** [add_output output value map] binds [value] under [output]. *)

  val find_output : 'a output -> map -> 'a option
  (** [find_output output map] returns the value bound under [output], if present. *)
end
