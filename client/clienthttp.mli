open Controller
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Yojson.Basic

(* The json type used to pass information in the body of the client *)
type json = Yojson.Basic.json
type diff = Controller.diff

(* [translate_to_json d] returns a json based on a diff *)
val translate_to_json: diff -> json

(* [translate_to_diff j] returns diffs based on a json *)
val translate_to_diff: json -> diff list

(* [send_json j} sends a json to the servers. Returns unit *)
val send_json: json -> unit
