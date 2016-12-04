open Yojson.Basic.Util


let debugging = true
let pr msg = if debugging then print_endline msg else ignore ()


(* locations of rooms are in cartesian coordinate with the bottom-left cell
 * being (0,0). X-axis is horizontal and increases value moving to the right.
 * Y-axis increases value ass moving up. *)
type room_loc = int * int

(* A map module that uses room locations to look up properties and contents
 * of a room. See [type world] for more details. *)
module RoomMap = Map.Make (
  struct
    type t = room_loc
    let compare (x1,x2) (y1,y2) =
      if compare x1 y1 = 0 then compare x2 y2 else compare x1 y1
  end )

exception ApplyDiffError of string

(* A library module uses ids to look up properties and contents
 * of an item.
 *
 * Indices of items are as follow:
 *   potions and spells: 0-99
 *   animals and police: 100-999
 *   players: 1000+
 *
 * See [type world] for more details. *)
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
 * effect = Turn 1 *)
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
  items : int list;
}

type world = {
  rooms: room RoomMap.t;
  players: (int * room_loc) list;
  items: item LibMap.t
}

type constructing_ai_lib = item LibMap.t

type diffparam = {loc: room_loc; id: int; newitem: item}

type diff =
  | Add of diffparam
  | Remove of diffparam
  | Change of diffparam

let string_of_inventory inv =
  let rec string_list str = function
    [] -> str
    | h::t -> string_list (str^(string_of_int h)^",") t
  in
  (string_list "" inv)

let string_of_item i =
  match i with
  | IPlayer p -> begin
      "Player:"^
      "\n\tID:"^(string_of_int p.id)^
      "\n\tName:"^(p.name)^
      "\n\tHP:"^(string_of_int p.hp)^
      "\n\tScore:"^(string_of_int p.score)^
      "\n\tInventory:"^(string_of_inventory p.inventory)
  end
  | IAnimal a -> begin
      "Beast:"^
      "\n\tID:"^(string_of_int a.id)^
      "\n\tName:"^(a.name)^
      "\n\tDescr:"^(a.descr)^
      "\n\tHP:"^(string_of_int a.hp)^
      "\n\tSpells:"^(string_of_inventory a.spells)
    end
  | IPolice a -> begin
      "Beast:"^
      "\n\tID:"^(string_of_int a.id)^
      "\n\tName:"^(a.name)^
      "\n\tDescr:"^(a.descr)^
      "\n\tHP:"^(string_of_int a.hp)^
      "\n\tSpells:"^(string_of_inventory a.spells)
    end
  | ISpell s -> begin
      "Spell:"^
      "\n\tID:"^(string_of_int s.id)^
      "\n\tIncant:"^(s.incant)^
      "\n\tDescr:"^(s.descr)^
      "\n\tEffect:"^(string_of_int s.effect)
    end
  | IPotion s -> begin
      "Potion:"^
      "\n\tID:"^(string_of_int s.id)^
      "\n\tName:"^(s.name)^
      "\n\tDescr:"^(s.descr)^
      "\n\tEffect:"^(string_of_int s.effect)
    end
  | IVoid -> "void"

let string_of_diff d =
  let item = match d with
    | Add x | Remove x | Change x -> string_of_item x.newitem
  in

  let id = match d with
    | Add x -> "ADD " ^ (string_of_int x.id)
    | Remove x -> "RMV " ^ (string_of_int x.id)
    | Change x -> "CHG " ^ (string_of_int x.id)
  in

  let (curx, cury) = match d with
    | Add x | Remove x | Change x -> x.loc
  in
  let printloc = "("^string_of_int curx^", " ^string_of_int cury^")" in

  ("Diff: " ^ id ^ " at "^printloc^" to "^item)

let string_of_diff_simple d = match d with
  | Add x -> "ADD " ^ (string_of_int x.id)
  | Remove x -> "RMV " ^ (string_of_int x.id)
  | Change x -> "CHG " ^ (string_of_int x.id)

let string_of_difflist client_diffs =
  let rec difflist str (id, lst) =
    List.fold_left (fun a b -> a^", "^(string_of_diff_simple b))
      ("["^(string_of_int id)^"]") lst
  in
  List.fold_left difflist "client_diffs:\t" client_diffs

    (* str ^"["^(string_of_int id)^"]"^
    (string_of_diff_simple diff)^",\n" *)

let print_libmap lmap =
  pr "---------------------------";
  LibMap.iter (fun index item->
      pr (Printf.sprintf "* Index: %s\n  Item: %s"
                       (string_of_int index)
                       (string_of_item item);)) lmap;
  pr "---------------------------"



(* [remove_item_from_list x i] removes item [i] from list [x].
 * If [x] does not contain [i], returns [x] *)
let rec remove_item_from_list x = function
  | h::t -> if h = x then t else h::(remove_item_from_list x t)
  | [] -> []

(* updates loc of player [id]. If no such player [id] is found,
 * append (id, new_loc) to [players] *)
let rec update_players (id: int) (new_loc: room_loc)
    (players: (int * room_loc) list) : (int * room_loc) list =
  match players with
  | (id', old_loc) as h::t ->(if id = id' then (id, new_loc)::t
    else h::(update_players id new_loc t))
  | [] -> (id, new_loc)::[]

let fnull_int () = -1

let fnull_string () = ""

let fnull_list () = [(-1)]

let null_int = fnull_int ()

let null_string = fnull_string ()

let null_list = fnull_list ()

let is_null i x = i = x

let is_null_int = is_null null_int

let is_null_string = is_null null_string

let is_null_list = is_null null_list

let unwrap_player = function
  | IPlayer x -> x
  | _ -> raise (ApplyDiffError "wrong item type" )

let unwrap_animal = function
  | IAnimal x -> x
  | _ -> raise (ApplyDiffError "wrong item type" )

let unwrap_police = function
  | IPolice x -> x
  | _ -> raise (ApplyDiffError "wrong item type" )

let unwrap_spell = function
  | ISpell x -> x
  | _ -> raise (ApplyDiffError "wrong item type" )

let unwrap_potion = function
  | IPotion x -> x
  | _ -> raise (ApplyDiffError "wrong item type" )

let complete_item_player (w: world) (i: player) : item =
  (* print_endline "Completing player..."; *)
  if LibMap.mem i.id w.items then
    let old_item = unwrap_player (LibMap.find (i.id) w.items) in
    (* print_endline (if is_null_string i.name then old_item.name else i.name); *)
    IPlayer ({
        id = i.id;
        name = if is_null_string i.name then old_item.name else i.name;
        hp = if is_null_int i.hp then old_item.hp else i.hp;
        score = if is_null_int i.hp then old_item.score else i.score;
        inventory = if is_null_list i.inventory then old_item.inventory
          else i.inventory;
      })
  else IPlayer(i)

let complete_item_animal (w: world) (i: ai) : item =
  let old_item = unwrap_animal (LibMap.find (i.id) w.items) in
  IAnimal ({
      id = i.id;
      name = if is_null_string i.name then old_item.name else i.name;
      descr = if is_null_string i.descr then old_item.descr else i.descr;
      hp = if is_null_int i.hp then old_item.hp else i.hp;
      spells = if is_null_list i.spells then old_item.spells else i.spells;
    })

let complete_item_police (w: world) (i: ai) : item =
  let old_item = unwrap_police (LibMap.find (i.id) w.items) in
  IPolice ({
      id = i.id;
      name = if is_null_string i.name then old_item.name else i.name;
      descr = if is_null_string i.descr then old_item.descr else i.descr;
      hp = if is_null_int i.hp then old_item.hp else i.hp;
      spells = if is_null_list i.spells then old_item.spells else i.spells;
    })

let complete_item_spell (w: world) (i: spell) : item =
  let old_item = unwrap_spell (LibMap.find (i.id) w.items) in
  ISpell ({
      id = i.id;
      incant = if is_null_string i.incant then old_item.incant else i.incant;
      descr = if is_null_string i.descr then old_item.descr else i.descr;
      effect = if is_null_int i.effect then old_item.effect else i.effect;
    })

let complete_item_potion (w: world) (i: potion) : item =
  let old_item = unwrap_potion (LibMap.find (i.id) w.items) in
  IPotion ({
      id = i.id;
      name = if is_null_string i.name then old_item.name else i.name;
      descr = if is_null_string i.descr then old_item.descr else i.descr;
      effect = if is_null_int i.effect then old_item.effect else i.effect;
    })

(* [complete_item w item] fills up missing fields in [item] by
 * taking values from [w] *)
let complete_item w = function
  | IPlayer i -> complete_item_player w i
  | IAnimal i -> complete_item_animal w i
  | IPolice i -> complete_item_police w i
  | ISpell i -> complete_item_spell w i
  | IPotion i -> complete_item_potion w i
  | IVoid -> IVoid

(* [apply_diff_case d new_items w f] is helper function for apply_diff_add,
 * apply_diff_remove, and apply_diff_change *)
let apply_diff_case (d: diffparam) (new_items: item LibMap.t) (w: world)
    (f: int -> int list -> int list) : world =
  let loc = d.loc in
  let id_to_edit = d.id in
  let curr_rooms = RoomMap.find loc w.rooms in
  let new_room =
    {curr_rooms with items = f id_to_edit curr_rooms.items} in

  let new_rooms = RoomMap.add loc new_room w.rooms in
  let updated_players =
    if id_to_edit >= 1000 then (update_players id_to_edit loc w.players)
    else w.players in
  {rooms = new_rooms; players = updated_players; items = new_items}

(* [apply_diff_change d w] adds [d] in [w] and returns new world.
 * If [w] does not contain [d], it adds [d] to [w] and returns new world *)
let apply_diff_add (d: diffparam) (w: world) : world =
  (* pr "Adding..."; *)
  let item_to_edit = complete_item w d.newitem in
  (* pr ("Got complete item "^(string_of_item item_to_edit)); *)
  (* print_libmap w.items; *)
  let new_items = LibMap.add d.id item_to_edit w.items in

  (* print_libmap w.items; *)
  apply_diff_case d new_items w (fun x y -> x::y)

(* [apply_diff_change d w] removes [d] in [w] and returns new world *)
let apply_diff_remove (d: diffparam) (w: world) : world =
  let new_items = match d.newitem with
    | IAnimal _ | IPolice _ -> LibMap.remove d.id w.items
    | ISpell _ | IPotion _ | IVoid -> w.items
    | IPlayer p ->
      if p.hp <= 0 then
        begin
          pr ("found a ghost "^ string_of_int p.hp);
          let ghost = {p with name = p.name ^ "'s ghost'"} in
          let newitems = LibMap.remove d.id w.items in
          pr (string_of_item (IPlayer ghost));
          let newlibmap = LibMap.add d.id (IPlayer ghost) newitems in
          (* print_libmap newlibmap; *)
          newlibmap
        end
      else
        w.items in
  let id_to_edit = d.id in
  let loc = d.loc in
  let curr_rooms = RoomMap.find loc w.rooms in
  let new_room =
    {curr_rooms with
     items = remove_item_from_list id_to_edit curr_rooms.items} in
  let new_rooms = RoomMap.add loc new_room w.rooms in
  {rooms = new_rooms; players = w.players; items = new_items}

(*
  pr "Removing...";
  let new_items = match d.newitem with
    | IAnimal _ | IPolice _ -> LibMap.remove d.id w.items
    | ISpell _ | IPotion _ | IVoid -> w.items
    | IPlayer p ->
      if p.hp <= 0 then
        begin
          pr ("found a ghost "^ string_of_int p.hp);
          let ghost = {p with name = p.name ^ "'s ghost'"} in
          let newitems = LibMap.remove d.id w.items in
          pr (string_of_item (IPlayer ghost));
          let newlibmap = LibMap.add d.id (IPlayer ghost) newitems in
          print_libmap newlibmap;
          newlibmap
        end
      else
        w.items
  in
  apply_diff_case d new_items {w with items = new_items}
    remove_item_from_list *)

(* [apply_diff_change d w] changes [d] in [w] and returns new world
 * If [w] does not contain [d], it adds [d] to [w] and returns new world *)
let apply_diff_change (d: diffparam) (w: world) : world =
  apply_diff_add d w

(* [apply_diff_helper d w] is the same as apply_diff except it might raise
 * different exception messages *)
let rec apply_diff_helper (d: diff) (w: world) : world =
  match d with
  | Add x -> apply_diff_add x w
  | Remove x -> apply_diff_remove x w
  | Change x -> apply_diff_change x w

(* [apply_diff d w] takes in a difference and returns an updated
 * minimodel based on the diff *)
let rec apply_diff (d: diff) (w: world) : world =
  try
    print_endline ("Applying diff " ^(string_of_diff_simple d));
    apply_diff_helper d w
  with
  | _ -> raise (ApplyDiffError "incompatible with the current world")

let init size =
  let room00 = {descr="This is a room!"; items = [1;2;1234]} in
  let room10 = {descr="This is a room!"; items = [2;3;100;101]} in
  let room01 = {descr="This is a room!"; items = [1;3]} in
  let room11 = {descr="This is a room!"; items = [1;1]} in
  let map = RoomMap.empty |> RoomMap.add (0,0) room00
            |> RoomMap.add (1,0) room10
            |> RoomMap.add (0,1) room01
            |> RoomMap.add (1,1) room11 in
  let players = [(1234, (0,0))] in
  let items = LibMap.empty
              |> LibMap.add 1 (ISpell {id = 1;
                                       incant = "lumos";
                                       descr = "a light spell";
                                       effect = 10})
              |> LibMap.add 2 (ISpell {id = 2;
                                       incant = "avada kedavra";
                                       descr = "a death spell";
                                       effect = -1000})
              |> LibMap.add 3 (IPotion {id = 3;
                                       name = "pepperup potion";
                                        descr = "warms and energizes";
                                        effect = 30})
              |> LibMap.add 10 (ISpell {id = 10;
                                       incant = "bite";
                                       descr = "ouch";
                                        effect = -20})
              |> LibMap.add 11 (ISpell {id = 11;
                                        incant = "transform";
                                        descr = "a scary looking thing";
                                        effect = -10})
              |> LibMap.add 100 (IAnimal {id = 100;
                                         name = "boggart";
                                         descr = "pretty scary";
                                         hp = 50;
                                         spells= [10;11]})
              |> LibMap.add 101 (IAnimal {id = 101;
                                         name = "boggart";
                                         descr = "pretty scary";
                                         hp = 50;
                                         spells= [10;11]})
              |> LibMap.add 1234 (IPlayer {id = 1234;
                                           name = "rebecca";
                                           hp = 1000;
                                           score = 100;
                                           inventory = [1;2;2;2;2;2;3;3]})
  in
  {
    rooms = map;
    players = players;
    items = items;
  }

let add_room room_map room_json =
  let loc = room_json |> member "loc" in
  let x = loc |> member "x" |> to_int in
  let y = loc |> member "y" |> to_int in
  let items = room_json |> member "items" |> to_list |> List.map to_int in
  let des = room_json |> member "descr" |> to_string in
  let room = {descr = des; items = items} in
  RoomMap.add (x,y) room room_map


let add_spell item_map item_json =
  let id = item_json |> member "id" |> to_int in
  let incantation = item_json |> member "incant" |> to_string in
  let description = item_json |> member "descr" |> to_string in
  let effect = item_json |> member "effect" |> to_int in
  let spell = ISpell {id = id; incant = incantation; descr = description ;
                      effect = effect} in
  LibMap.add id spell item_map

let add_potion item_map item_json =
  let id = item_json |> member "id" |> to_int in
  let name = item_json |> member "name" |> to_string in
  let descr = item_json |> member "descr" |> to_string in
  let effect = item_json |> member "effect" |> to_int in
  let potion = IPotion {id = id; name = name; descr = descr; effect = effect} in
  LibMap.add id potion item_map

let add_player item_map item_json =
  let id = item_json |> member "id" |> to_int in
  let name = item_json |> member "name" |> to_string in
  let hp = item_json |> member "hp" |> to_int in
  let score = item_json |> member "score" |> to_int in
  let inv = item_json |> member "inv" |> to_list |> List.map to_int in
  let player = IPlayer
      {id = id; name = name; hp = hp; score = score; inventory = inv} in
  LibMap.add id player item_map

let add_police item_map item_json =
  let id = item_json |> member "id" |> to_int in
  let name = item_json |> member "name" |> to_string in
  let hp = item_json |> member "hp" |> to_int in
  let descr = item_json |> member "descr" |> to_string in
  let spells = item_json |> member "spells" |> to_list |> List.map to_int in
  let ai = IPolice
      {id = id; name = name; hp = hp; descr = descr; spells = spells} in
  LibMap.add id ai item_map

let add_beast item_map item_json =
  let id = item_json |> member "id" |> to_int in
  let name = item_json |> member "name" |> to_string in
  let hp = item_json |> member "hp" |> to_int in
  let descr = item_json |> member "descr" |> to_string in
  let spells = item_json |> member "spells" |> to_list |> List.map to_int in
  let ai = IAnimal
      {id = id; name = name; hp = hp; descr = descr; spells = spells} in
  LibMap.add id ai item_map

let add_item item_map item_json =
  let item_type = item_json |> member "item type" |> to_string in
  match item_type with
  | "spell" -> add_spell item_map item_json
  | "potion" -> add_potion item_map item_json
  | "player" -> add_player item_map item_json
  | "police" -> add_police item_map item_json
  | "animal" -> add_beast item_map item_json
  | _ -> failwith "invalid item type"

let make_player player_json =
  let id = player_json |> member "id" |> to_int in
  let x = player_json |> member "x" |> to_int in
  let y =  player_json |> member "y" |> to_int in
  (id, (x,y))

let init_state j =
  let orig_room = RoomMap.empty in
  let orig_item = LibMap.empty in
  let actual_rooms = j |> member "rooms" |> to_list |>
                     List.fold_left add_room orig_room in
  let items = j |> member "items" |> to_list |>
              List.fold_left add_item orig_item in
  let player_lst = j |> member "players" |> to_list |>
                   List.map make_player in
  {rooms = actual_rooms; players = player_lst; items = items}
