open model 

type model = world

(* [apply_diff d] takes in a difference and returns an updated
 * minimodel based on the diff.*)
val apply_diff: diff -> minimodel

val interpret_command: command -> diff