(* A map module that uses room locations to look up properties of and contents
 * of a room. See [type world] for more details. *)
module RoomMap
module LibMap

(* A spell is casted to act on an object. However, there are consequences of
 * casting specific spells.
 * An example of a spell:
 * incantation = "Expelliarmus"
 * description = "disarms your opponent"
 * effect = Turn 1
 * consequence = None
 * environment = None *)
type spell = {
  id : int;
  incant: string;
  descr : string;
  effect : int;
}

type potion = {
  id : int;
  descr : string;
  effect : int;
}

(* fields that can be updated in a move *)
type player = {
  id : int;
  hp : int;
  score : int;
  inventory : int list;
}

type ai = {
  id : int;
  name : string;
  descr : string;
  hp : int;
  spells : int list;
}

(* A type that is one of several records, all of which contain enough
 * information to represent both the static and dynamic parts of an item.
 * For a spell, for example, it is sufficient to know what type of spell it is;
 * all spells with the same int identifier have the same effect..
 * For an animal, it is necessary to know the static info like its starting HP,
 * and dynamic info, like its current HP. See type [mut_AI] for more. *)
type item =
  | IPlayer of player
  | IAnimal of ai
  | IPolice of ai
  | ISpell of spell
  | IPotion of potion

(* each room has location row by column based on 50x50 system.
 * The description includes how the room looks like but not the items
 * in the room. *)
type room = {
  descr : string;
  items : int list;
}

type world = {
  rooms: room RoomMap.t;
  player: (int * room_loc) list;
  items: item LibMap.t
}

type diffparam = {loc : room_loc; newitem : item};

type diff =
  | Add of diffparam
  | Remove of diffparam
  | Change of diffparam

(* [apply_diff d] takes in a difference and returns an updated
 * minimodel based on the diff.*)
val apply_diff: diff -> world

