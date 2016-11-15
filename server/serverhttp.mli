(* The json type used to pass information in the body fo the client *)
type json
type status

(* [translate_to_newuser j] will attempt to add the new user with the password
 * they have included in [j], returning a sessionid if valid. *)
val translate_to_newuser: json -> int

(* [translate_to_diff j] returns diffs based on a json *)
val translate_to_diff: json -> Controller.diff list

(* [translate_to_json d] returns a json based on diffs *)
val translate_to_json: Controller.diff list -> json

(* [send_response j status] sends an http response to the clients *)
val send_respoonse: json -> status -> unit

(* [send_status] sense a response without a body*)
val send_status: status -> unit
