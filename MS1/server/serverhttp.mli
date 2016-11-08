open Controller

type status = OK | Invalid

(* [translate_to_diff j] returns diffs based on a json *)
val translate_to_diff: json -> diff list

(* [translate_to_json d] returns a json based on diffs *)
val translate_to_json: diff list -> json

(* [send_json j] sends a json to the clients. Returns unit *)
val send_update: json -> unit

(* [send_signal] lets clients know if the request is valid *)
val send_signal: status -> unit
