open Model
open Controller

(* recieves a status from the serverhttp, which indicates if the
 * user's command was valid or not. If the status is Okay, then
 * that means the user's command was valid, so returns true,
 * else if the statis is Invalid, then the command was invalid, so
 * returns false. *)
val receive_status: status -> bool

(* [translate_to_json d] returns a json based on a diff *)
val translate_to_json: diff -> json

(* [translate_to_diff j] returns a diff based on a json *)
val translate_to_diff: json -> diff

(* [send_json j} sends a json to the servers. Returns unit *)
val send_json: json -> unit
