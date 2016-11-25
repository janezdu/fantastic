open Model
open Cli
open Clienthttp

type command
type diff
type diff_json
type comm_json

(* [translate_to_diff j] returns diffs based on a diff json string *)
val translate_to_diff: diff_json -> diff list

(* [translate_to_json d] returns a command json string based on diffs *)
val interpret_command: command -> comm_json
