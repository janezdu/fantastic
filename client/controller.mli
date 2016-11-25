open Model
open Cli
open Clienthttp

type command = Cli.command
type diff = Model.diff
type jsonstring = string

(* [translate_to_json d] returns a command json string based on diffs *)
val translate_to_json: command -> jsonstring

(* [translate_to_diff j] returns diffs based on a diff json string *)
val translate_to_diff: jsonstring -> diff list