open Model
open Cli

(* [diff] represents changes that are made in a player's turn.
 * Invariant: [dplayers] and [ditems] only store players and rooms that change.
 * Steady rooms and players must not be included in a [diff]. *)
type diff = {
  ditems : room_loc * (diff_item list) list option;
}


(* [apply_diff d] takes in a difference and returns an updated
 * minimodel based on the diff.*)
let apply_diff d = failwith "unimplemented"

(* [interpret_command c] returns a diff based on a command*)
let interpret_command c = failwith "unimplemented"