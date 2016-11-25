
(* directive is what the player types into the command line*)
type directive

(* command is a variant type of the possible commands a player
 * could make *)
type command =
  | Move of string
  | Drink of string
  | Spell of string
  | Quit
  | Take of string
  | Drop of string
  | Look
  | Inventory
  | ViewState
  | Help

(* [parse_comm d] is the command type that results from the player's
 * typed directrive. *)

val parse_comm: directive -> command

