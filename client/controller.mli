open Model
open Cli
open Clienthttp


type command = Cli.command
type diff = Model.diff
type diff_json = Clienthttp.diff_json

type comm_json =
  | JMove of string
  | JDrink of string
  | JSpell of string
  | JQuit
  | JTake of string
  | JDrop of string
  | JLook
  | JInv
  | JViewState
  | JHelp


(* [translate_to_diff j] returns diffs based on a diff json string *)
val translate_to_diff: diff_json -> diff list


(* [translate_to_json d] returns a command json string based on diffs *)
val interpret_command: command -> int -> Model.world -> comm_json

val repl: int -> world -> unit
