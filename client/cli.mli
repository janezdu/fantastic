open model  
open Controller

type directive = string

type command = Move of string | Spell of string | Do of string

(* [parse_comm d] is the command type that results from the player's
 * typed directrive. *)
val parse_comm: directive -> command

(* [view_world m] returns a string description of the world to the 
 * player based on the minimodel of the game*)
val view_world: minimodel -> string

