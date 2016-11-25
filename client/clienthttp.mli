open Controller
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Yojson.Basic

(* The json type used to pass information in the body of the client *)
type diff = Controller.diff
type jsonstring = string

(* [translate_to_json d] returns a json based on diffs, to send to server *)
val translate_to_json: diff -> jsonstring

(* [translate_to_diff j] returns diffs based on a json string, which is
 * a response from server *)
val translate_to_diff: jsonstring -> diff list

(* [send_post_request j} sends a json to the servers. Returns unit *)
val send_post_request: jsonstring -> string -> (string -> string) -> unit

(* [send_post_request j} sends a json to the servers. Returns unit *)
val send_get_request: jsonstring -> string  -> (string -> string) -> unit
