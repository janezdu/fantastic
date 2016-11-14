(* [timeid] is timestamp of a gamestate aka [world]
 * the initial world has timeid 0. When the first player makes a change, the
 * most up-to-date world has timestamp 1, and so on *)
type timeid 

(* location of a room in grid system *)
type room_loc

(* difference that can occur in a room *)
type diff_item

(* Explanation:
 * [world] represents a game state
 * [wid] is the most up-to-date timestamp [world] is
 * A world contains information about room map [wrooms],
 * spell library [wspells], potion library [wpotions], animal library [animals],
 * police library [wpolice], player library [wplayers],
 * and dictionary of room associated with items in the room [witems] *)
type world

(* Takes in a diff and a world, and updates the world by applying the changes
 * from the diff *)
val applydiff : world -> diff -> world

(* [validate w d] returns true if applying [d] to [w] is legal, false ow*)
val validate: world -> diff -> bool
