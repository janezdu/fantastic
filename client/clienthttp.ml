open Controller
open Yojson.Basic.Util

type json = Yojson.Basic.json
type diff = Controller.diff

(* [translate_to_json d] returns a json based on diffs *)
let translate_to_json d = failwith "unimplemented"

(* [translate_to_diff j] returns diffs based on a json *)
let translate_to_diff j = failwith "unimplemented"

(* [send_json j} sends a json to the servers. Returns unit *)
let send_json j = failwith "unimplemented"
