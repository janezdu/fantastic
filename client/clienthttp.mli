open Controller
open Lwt
open Cohttp
open Cohttp_lwt_unix

type diff = Controller.diff
type jsonstring = string

(* [translate_to_json d] returns a command json string based on diffs *)
val translate_to_json: diff -> jsonstring

(* [translate_to_diff j] returns diffs based on a diff json string *)
val translate_to_diff: jsonstring -> diff list

(* [send_post_request j} sends a command json string to the servers.
 * Returns unit *)
val send_post_request: jsonstring -> string -> (string -> string) -> unit

(* [send_post_request j} sends a get request to the servers. Returns unit *)
val send_get_request: jsonstring -> string  -> (string -> string) -> unit
