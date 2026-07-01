module type S = sig
  val model : string

  val forward :
    sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    Lm_request.t ->
    Lm_response.t
end
