open Controller
open Lwt
open Model
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Yojson.Basic

(* The json type used to pass information in the body of the client *)
type json = Yojson.Basic.json
type diff = Model.diff

(* [translate_to_json d] returns a json based on diffs *)
val translate_to_json: diff -> json

(* [translate_to_diff j] returns diffs based on a json *)
val translate_to_diff: json -> diff list

(* [send_json j} sends a json to the servers. Returns unit *)
val send_json: string -> unit
