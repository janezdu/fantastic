open Model

(* Takes in a diff and a world, and updates the world by applying the changes
 * from the diff *)
val applydiff : world -> diff -> world

(* [validate w d] returns true if applying [d] to [w] is legal, false ow*)
val validate: world -> diff -> bool
