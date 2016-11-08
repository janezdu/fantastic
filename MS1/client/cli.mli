open Model
open Controller

(* directive is what the player types into the command line*)
type directive = string

(* command is a variant type of the possible commands a player
 * could make *)
type command =
  | Move of string
  | Spell of string
  | Quit
  | Take of string
  | Drop of string
  | Look
  | Inventory
  | Drink of string
  | ViewState
  | Help

(* [parse_comm d] is the command type that results from the player's
 * typed directrive. *)
val parse_comm: directive -> command

(* [view_world m] returns a string description of the world to the
 * player based on the minimodel of the game*)
val view_world: world -> unit

