(** Typed codecs for values that appear in prompts and model responses.

    A value type knows how to render a value into prompt text, parse text back
    from an LM response, and convert to/from JSON for provider-facing formats. *)

type 'a t
(** A codec for values of type ['a]. *)

val name : 'a t -> string
(** [name typ] is the human-readable type name used in prompt descriptions. *)

val render : 'a t -> 'a -> string
(** [render typ value] converts [value] to text suitable for prompt input.
    Exceptions raised by a custom renderer are propagated. *)

val parse : 'a t -> string -> ('a, string) result
(** [parse typ text] parses model output text as [typ]. Exceptions raised by a
    custom parser are propagated. *)

val parse_exn : field:string -> 'a t -> string -> 'a
(** [parse_exn ~field typ text] is [parse typ text], raising
    {!Dspy_error.Error} with [field] in the message on failure.

    @raise Dspy_error.Error if [parse typ text] returns [Error _]. Exceptions
    raised by a custom parser are propagated. *)

val to_json : 'a t -> 'a -> Yojson.Safe.t
(** [to_json typ value] converts [value] to JSON. Exceptions raised by a custom
    JSON encoder are propagated. *)

val of_json : 'a t -> Yojson.Safe.t -> ('a, string) result
(** [of_json typ json] decodes a JSON value as [typ]. Exceptions raised by a
    custom JSON decoder are propagated. *)

val make :
  name:string ->
  render:('a -> string) ->
  parse:(string -> ('a, string) result) ->
  to_json:('a -> Yojson.Safe.t) ->
  of_json:(Yojson.Safe.t -> ('a, string) result) ->
  'a t
(** [make ~name ~render ~parse ~to_json ~of_json] builds a custom value type. *)

val string : string t
(** Strings, rendered and parsed as plain text. *)

val int : int t
(** Integers, parsed with [int_of_string] after trimming whitespace. *)

val float : float t
(** Floating-point numbers, parsed with [float_of_string] after trimming whitespace. *)

val bool : bool t
(** Booleans, parsed from ["true"] or ["false"] case-insensitively. *)

val json : Yojson.Safe.t t
(** JSON values, rendered as pretty JSON and parsed from JSON text. *)

val list : 'a t -> 'a list t
(** [list item] is a JSON-array codec for lists of [item]. The resulting codec
    propagates exceptions raised by [item]'s JSON encoder or decoder. *)
