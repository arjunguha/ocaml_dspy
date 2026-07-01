(** Typed heterogeneous input values. *)

type t
(** A map from {!type:Signature.input} witnesses to values. *)

val empty : t
(** The empty input map. *)

val set : 'a Signature.input -> 'a -> t -> t
(** [set input value values] binds [input] to [value]. *)

val get : 'a Signature.input -> t -> 'a option
(** [get input values] returns the value bound to [input], if present. *)

val get_exn : 'a Signature.input -> t -> 'a
(** [get_exn input values] returns [input]'s value.

    @raise Dspy_error.Error if [values] has no value for [input]. *)

val validate : Signature.t -> t -> unit
(** [validate signature values] checks that [values] contains exactly values for
    [signature]'s inputs.

    @raise Dspy_error.Error if [values] contains a field that is not an input of
    [signature] or omits an input required by [signature]. *)
