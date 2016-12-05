open Model
open Cli
open Clienthttp
open Lwt

exception NotAnItem
exception Illegal
exception Dead

type json = Yojson.Basic.json

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
  | JCheck
  | JCheckout of string 

val client_id: int ref

val init_state: json -> world

val get_hp: int -> item LibMap.t -> int

val get_score: int -> item LibMap.t -> int

val get_curr_loc: (int * room_loc) list -> (int * int)

(* [translate_to_diff j] returns diffs based on a diff json string *)
val translate_to_diff: diff_json -> diff list

val translate_to_client_id: diff_json -> unit

(* [translate_to_json d] returns a command json string based on diffs *)
val interpret_command: string -> int -> world -> comm_json

(* [do_command comm current_player world] requests to server for [comm]
 * that needs a server update, and pull info from the client's world
 * for commands that doesn't need server connection. *)
val do_command: string -> int -> world -> (int * string Lwt.t) Lwt.t

val repl_helper : string -> world -> world Lwt.t

(* [loadin ()] starts the game. It is the entry point of this module *)
val loadin: unit -> world Lwt.t