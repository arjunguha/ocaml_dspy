type t = {
  adapter : Chat_adapter.t;
  config : Lm_request.config;
  lm : (module Lm.S);
  signature : Signature.t;
}

let create ?(adapter = Chat_adapter.default) ?(config = Lm_request.default_config) ~lm signature =
  { adapter; config; lm; signature }

let forward ~sw ~env t values =
  Values.validate t.signature values;
  let messages = Chat_adapter.render t.adapter t.signature values in
  let module Lm = (val t.lm : Lm.S) in
  let request = Lm_request.make ~model:Lm.model ~config:t.config messages in
  let response = Lm.forward ~sw ~env request in
  Chat_adapter.parse t.adapter t.signature (Lm_response.text response)
