open Model

(* Takes in a diff and a world, and updates the world by applying the changes
 * from the diff *)
let applydiff w d: world -> diff -> world = 
    failwith "unimplemented"

    
(* [validate w d] returns true if applying [d] to [w] is legal, false ow*)
let validate w d: world -> diff -> bool =
    failwith "unimplemented"
