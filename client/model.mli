type room_loc = int * int

(* A map module that uses room locations to look up properties of and contents
 * of a room. See [type world] for more details. *)
module RoomMap : sig
  type key = room_loc
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val remove : key -> 'a t -> 'a t
  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal : 'a t -> int
  val bindings : 'a t -> (key * 'a) list
  val min_binding : 'a t -> key * 'a
  val max_binding : 'a t -> key * 'a
  val choose : 'a t -> key * 'a
  val split : key -> 'a t -> 'a t * 'a option * 'a t
  val find : key -> 'a t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
end

module LibMap : sig
  type key = int
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val remove : key -> 'a t -> 'a t
  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val compare : ('a -> 'a -> key) -> 'a t -> 'a t -> key
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal : 'a t -> key
  val bindings : 'a t -> (key * 'a) list
  val min_binding : 'a t -> key * 'a
  val max_binding : 'a t -> key * 'a
  val choose : 'a t -> key * 'a
  val split : key -> 'a t -> 'a t * 'a option * 'a t
  val find : key -> 'a t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
end

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
  name: string;
  descr : string;
  effect : int;
}

(* fields that can be updated in a move *)
type player = {
  id : int;
  name: string;
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
  | IVoid

(* each room has location row by column based on 50x50 system.
 * The description includes how the room looks like but not the items
 * in the room. *)
type room = {
  descr : string;
  items : item list;
}

type world = {
  rooms: room RoomMap.t;
  player: (int * room_loc) list;
  items: item LibMap.t
}

type diffparam = {loc: room_loc; id: int; newitem: item}

type diff =
  | Add of diffparam
  | Remove of diffparam
  | Change of diffparam

(* [apply_diff d] takes in a difference and returns an updated
 * minimodel based on the diff.*)
val apply_diff: diff -> world -> world


