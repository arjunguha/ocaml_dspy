exception Error of string

let errorf fmt = Format.kasprintf (fun msg -> raise (Error msg)) fmt
