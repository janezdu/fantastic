open Model

(* [apply_diff d] takes in a difference and returns an updated
 * minimodel based on the diff.*)
val apply_diff: diff -> world

(* [interpret_command c] returns a diff based on a command*)
val interpret_command: command -> diff