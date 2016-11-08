open Controller
open Model

type status = OK | Invalid

(* [start pw] will start a game server; join game with password [pw] *)
val start: string -> serverstate

(* [translate_to_newuser j] will attempt to add the new user with the password
 * they have included in [j], returning a sessionid if valid. *)
val translate_to_newuser: json -> int

(* [translate_to_diff j] returns diffs based on a json *)
val translate_to_diff: json -> diff list

(* [translate_to_json d] returns a json based on diffs *)
val translate_to_json: diff list -> json

(* [send_response j status] sends an http response to the clients *)
val send_respoonse: json -> status -> unit

(* [send_status] sense a response without a body*)
val send_status: status -> unit
