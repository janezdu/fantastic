open Model

type command
type diff = Model.diff

(* [interpret_command c] returns a diff based on a command*)
val interpret_command: Cli.command -> diff