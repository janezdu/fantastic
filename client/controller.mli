<<<<<<< HEAD
open Model
open Cli
open Clienthttp

exception NotAnItem
exception Illegal

type command = Cli.command
type diff = Model.diff
type diff_json = string
type comm_json = string (*try to hide the type*)

(* [translate_to_diff j] returns diffs based on a diff json string *)
val translate_to_diff: diff_json -> diff list

(* [translate_to_json d] returns a command json string based on diffs *)
val interpret_command: command -> comm_json
