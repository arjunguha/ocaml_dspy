type t = {
  reasoning : string Signature.output;
  predict : Predict.t;
}

type result = {
  reasoning : string;
  prediction : Prediction.t;
}

let create ?adapter ?config ~lm signature =
  let reasoning : string Signature.output =
    Signature.output ~desc:"Step-by-step reasoning used to derive the answer."
      "reasoning" Type.string
  in
  let signature =
    Signature.make
      ~instructions:
        (Signature.instructions signature
        ^ "\nThink through the problem before producing the final outputs.")
      ~inputs:(Signature.inputs signature)
      ~outputs:(Signature.Packed_output reasoning :: Signature.outputs signature)
      ()
  in
  { reasoning; predict = Predict.create ?adapter ?config ~lm signature }

let forward ~sw ~env t values =
  let prediction = Predict.forward ~sw ~env t.predict values in
  { reasoning = Prediction.get t.reasoning prediction; prediction }
