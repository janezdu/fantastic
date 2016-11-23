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
  | ISpell of int
  | IPotion of int

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

type diffparam = {loc: room_loc; newitem: item}

type diff =
  | Add of diffparam
  | Remove of diffparam
  | Change of diffparam

let is_same_kind_item (x: item) (y: item) : bool =
  match x with
  | IPlayer x' ->
    begin
      match y with
      | IPlayer y' -> x'.id = y'.id
      | _ -> false
    end
  | IAnimal x' ->
    begin
      match y with
      | IAnimal y' -> x'.id = y'.id
      | _ -> false
    end
  | IPolice x' ->
    begin
      match y with
      | IPolice y' -> x'.id = y'.id
      | _ -> false
    end
  | ISpell _ | IPotion _ -> x = y

let remove_id_of_same_kind_from_list (x:item) (lst: item list) : item list =
  List.filter (fun i -> is_same_kind_item x i) lst

let remove_item_from_list (x:item) (lst: item list) : item list =
  List.filter (fun i -> i <> x) lst

let apply_diff_case (d: diffparam) (w: world)
  (f: item -> item list -> item list) : world =
  let loc = d.loc in
  let item_to_edit = d.newitem in
  let curr_rooms = RoomMap.find loc w.rooms in
  let new_room =
    {curr_rooms with items = f item_to_edit curr_rooms.items} in
  let new_rooms = RoomMap.add loc new_room w.rooms in
  {w with rooms = new_rooms}

let apply_diff_add (d: diffparam) (w: world) : world =
  apply_diff_case d w (fun x y -> x::y)

let apply_diff_remove (d: diffparam) (w: world) : world =
  apply_diff_case d w remove_item_from_list

(* assume that only player/ai call this function *)
let apply_diff_change (d: diffparam) (w: world) : world =
  let new_w = apply_diff_case d w remove_id_of_same_kind_from_list in
  apply_diff_add d new_w

let rec apply_diff_helper (d: diff) (w: world) : world =
  match d with
  | Add x -> apply_diff_add x w
  | Remove x -> apply_diff_remove x w
  | Change x -> apply_diff_change x w

(* [apply_diff d] takes in a difference and returns an updated
 * minimodel based on the diff.*)
let rec apply_diff (d: diff) (w: world) : world =
  try
    apply_diff_helper d w
  with
  | _ -> failwith "incompatible with the current world"
