(** Shared exception for user-facing DSPy runtime errors. *)

exception Error of string
(** Raised when signatures, values, predictions, parsing, or provider responses
    are invalid for the requested operation. *)

val errorf : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [errorf fmt ...] raises {!Error} with a formatted message.

    @raise Error always. *)
