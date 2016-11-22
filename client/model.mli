(* [timeid] is timestamp of a gamestate aka [world]
 * the initial world has timeid 0. When the first player makes a change, the
 * most up-to-date world has timestamp 1, and so on *)
(* type timeid *)

(* location of a room in grid system *)
type room_loc = int * int

type spell = {
  incant: string;
  descr : string;
  effect : int;
}

type potion = {
  descr : string;
  effect : int;
}

type inv =
  | IVSpell of spell
  | IVPotion of potion

(* fields that can be updated in a move *)
type player = {
  id : int;
  hp : int;
  score : int;
  inventory : inv list;
}

type ai = {
  id : int;
  name : string;
  descr : string;
  hp : int;
  spells : spell list;
}

type item =
  | IPlayer of player
  | IAnimal of ai
  | IPolice of ai
  | ISpell of spell
  | IPotion of potion

(* difference that can occur in a room *)
type diff_item = Remove of item | Add of item

(* Explanation:
 * [world] represents a game state
 * [wid] is the most up-to-date timestamp [world] is
 * A world contains information about room map [wrooms],
 * spell library [wspells], potion library [wpotions], animal library [animals],
 * police library [wpolice], player library [wplayers],
 * and dictionary of room associated with items in the room [witems] *)
type world

(* [diff] represents changes that are made in a player's turn.
 * Invariant: [dplayers] and [ditems] only store players and rooms that change.
 * Steady rooms and players must not be included in a [diff]. *)
type diff = {
  ditems : (room_loc * (diff_item list)) list ;
}

(* [apply_diff d] takes in a difference and returns an updated
 * minimodel based on the diff.*)
val apply_diff: diff -> world

