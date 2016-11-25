type room_loc = int * int

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
  | IVoid

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

type diffparam = {loc: room_loc; id: int; newitem: item}

type diff =
  | Add of diffparam
  | Remove of diffparam
  | Change of diffparam

(* [apply_diff d] takes in a difference and returns an updated
 * minimodel based on the diff.*)

let apply_diff_case (d: diffparam) (w: world) =
  failwith "unimplemented"
  (* (f: int -> int list -> int list) : world =
  let loc = d.loc in
  let id_to_edit = d.id in
  let item_to_edit = complete_item w d.newitem in
  let curr_rooms = RoomMap.find loc w.rooms in
  let new_room =
    {curr_rooms with items = f id_to_edit curr_rooms.items} in
  let new_rooms = RoomMap.add loc new_room w.rooms in
  let new_items = LibMap.add id_to_edit item_to_edit w.items in
  let updated_players =
    if id_to_edit >= 1000 then update_players id_to_edit loc w.players
    else w.players in
     {rooms = new_rooms; players = updated_players; items = new_items} *)

let apply_diff_add (d: diffparam) (w: world) : world =
  (* apply_diff_case d w (fun x y -> x::y)  *)
  failwith "unimplemented"

let apply_diff_remove (d: diffparam) (w: world) : world =
  failwith "unimpl"
  (* let loc = d.loc in
  let id_to_edit = d.id in
  let curr_rooms = RoomMap.find loc w.rooms in
  let new_room =
    {curr_rooms with items = remove_item_from_list id_to_edit curr_rooms.items} in
  let new_rooms = RoomMap.add loc new_room w.rooms in
  let new_items = LibMap.remove id_to_edit w.items in
  let updated_players =
    if id_to_edit >= 1000 then remove_players id_to_edit w.players
    else w.players in
  {rooms = new_rooms; players = updated_players; items = new_items} *)

let apply_diff_change (d: diffparam) (w: world) : world =
  let new_w = apply_diff_remove d w in
  apply_diff_add d new_w

let rec apply_diff_helper (d: diff) (w: world) : world =
  match d with
  | Add x -> apply_diff_add x w
  | Remove x -> apply_diff_remove x w
  | Change x -> apply_diff_change x w

(* [apply_diff d] takes in a difference and returns an updated
 * model based on the diff.*)
let rec apply_diff (d: diff) (w: world) : world =
  try
    apply_diff_helper d w
  with
  | _ -> failwith "incompatible with the current world"


(* [validate w d] returns true if applying [d] to [w] is legal, false ow*)
let validate w d: world -> diff -> bool =
    failwith "unimplemented"
