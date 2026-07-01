(** Typed heterogeneous output predictions. *)

type t
(** A map from {!type:Signature.output} witnesses to predicted values. *)

val empty : t
(** The empty prediction map. *)

val set : 'a Signature.output -> 'a -> t -> t
(** [set output value prediction] binds [output] to [value]. *)

val get : 'a Signature.output -> t -> 'a
(** [get output prediction] returns [output]'s value.

    @raise Dspy_error.Error if [prediction] has no value for [output]. *)

val get_opt : 'a Signature.output -> t -> 'a option
(** [get_opt output prediction] returns [output]'s value, if present. *)
