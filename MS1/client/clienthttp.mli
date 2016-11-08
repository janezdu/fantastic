open Model
open Controller

(* [translate_to_json d] returns a json based on a diff *)
val translate_to_json: diff -> json

(* [translate_to_diff j] returns a diff based on a json *)
val translate_to_diff: json -> diff

(* [send_json j} sends a json to the servers. Returns unit *)
val send_json: json -> unit
