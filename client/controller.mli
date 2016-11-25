type command
type comm_json
type diff

exception NotAnItem
exception Illegal

(* [interpret_command c] returns a diff based on a command*)
val interpret_command: command -> comm_json