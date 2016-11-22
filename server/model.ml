(* A map module that uses room locations to look up properties of and contents
 * of a room. See [type world] for more details. *)
module RoomMap = Map.Make (
  struct
    type t = room_loc
    let compare (x1,x2) (y1,y2) =
      if compare x1 y1 = 0 then compare x2 y2 else compare x1 y1
  end )

module LibMap = Map.Make (
    struct
      type t = int
      let compare e1 e2 = compare e1 e2
    end )


(* A spell is casted to act on an object. However, there are consequences of
 * casting specific spells.
 * An example of a spell:
 * incantation = "Expelliarmus"
 * description = "disarms your opponent"
 * effect = Turn 1
 * consequence = None
 * environment = None *)
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

type diff = json;

(* [apply_diff d] takes in a difference and returns an updated
 * minimodel based on the diff.*)
let apply_diff d = failwith "unimplemented"

(* [validate w d] returns true if applying [d] to [w] is legal, false ow*)
let validate w d: world -> diff -> bool =
    failwith "unimplemented"

