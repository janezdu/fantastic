(* The json type used to pass information in the body fo the client *)
type json 
type diff

(* [translate_to_json d] returns a json based on diffs *)
val translate_to_json: diff -> json

(* [translate_to_diff j] returns diffs based on a json *)
val translate_to_diff: json -> diff list

(* [send_json j} sends a json to the servers. Returns unit *)
val send_json: json -> unit
