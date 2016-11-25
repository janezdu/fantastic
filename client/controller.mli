type command = Cli.command
type comm_json
type diff

type world = Model.world

exception NotAnItem
exception Illegal

(* [interpret_command c] returns a diff based on a command*)
val interpret_command: string -> int -> world -> comm_json