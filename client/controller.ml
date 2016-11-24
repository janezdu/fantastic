open Model
open Cli
open Yojson.Basic.Util

type command = Cli.command
type diff = Model.diff
type command_json =
  | JMove of string
  | JDrink of string
  | JSpell of string
  | JQuit
  | JTake of string
  | JDrop of string
  | JLook
  | JInv
  | JViewState
  | JHelp


<<<<<<< HEAD
=======



>>>>>>> 92ac21c6a73059aece15cddce09ff28a70d449ca
exception NotAnItem
exception Illegal


let current_player_id = 1234

(* [init_state json] creates the inital world for the game *)
let init_state json = 
  let player_id = json |> member "player" |> to_string in
  let player_x = json |> member "x" |> to_int in
  let play_y = json |> member "y" |> to_int in
  (*let player = Player {id = player_id; name = ""; hp = 100; score = 0; inventory = []} in*)

   
  let rooms = RoomMap.(empty |> add {rdescr = "hi"; ritems = [player]}) in

  let players = [(player_id, (player_x, player_y))] in
  let items = LibMap.(empty ) in
  {rooms = rooms; player = players; items = items}

let rec remove i lst =
  match lst with 
  | [] -> []
  | h::t -> if h = i then t else h::(remove i t)


(* [update_world d] updates the world based on diff d*)
let update_world (d: diff) (w:world) = Model.apply_diff d w

  	

(* [step dl w] updates a world based on a diff list [dl]*)
let step (dl: diff list) (w: world) = 
  match dl with
  | [] -> w
  | h::t -> step t (update_world h w)

let check_match i key v= 
  match item_t with
  | IPlayer p -> if v = p.name then true else false
  | IAnimal a -> if v = a.name then true else false
  | IPolice a -> if v = a.name then true else false
  | ISpell s -> if v = s.encant then true else false
  | IPotion p -> if v = p.name then true else false
  | IVoid -> None

(*let rec find_in_lib i lst =
  match lst with
  | [] -> None
  | (key, item_t)::t -> *)

(* [find_item i w] finds item id based on name in world w*)
let find_item i w= 
  let items = world.items in
  (*let items_lst = LibMap.bindings items in*)
  LibMap.fold (fun k v acc -> if (check_match i k v) then Some k else acc) items None

  

(* [interp_move m w] returns a diff based on a move command m and world w*)
let interp_move (m:string) current_player w: command_json =
  match (String.lowercase_ascii m) with 
  | "north" -> 
    
  	let curr_loc = List.assoc current_player w.player in
  	let new_loc_x = fst curr_loc in
  	let new_loc_y = snd curr_loc + 1 in
  	JMove ("{\"new_x\":" ^ (string_of_int new_loc_x) ^  ", \"new_y\": " ^ 
  	(string_of_int new_loc_y) ^ "}")
  	
  | "south" -> 
    let curr_loc = List.assoc current_player w.player in
  	let new_loc_x = fst curr_loc in
    let new_loc_y = snd curr_loc - 1 in
    JMove ("{\"new_x\":" ^ (string_of_int new_loc_x) ^  ", \"new_y\": " ^ 
    (string_of_int new_loc_y) ^ "}")
  	
  | "east" -> 
    let curr_loc = List.assoc current_player w.player in
  	let new_loc_x = fst curr_loc + 1 in

	  let new_loc_y = snd curr_loc in
	  JMove ("{\"new_x\":" ^ (string_of_int new_loc_x) ^  ", \"new_y\": " ^ 
	  (string_of_int new_loc_y) ^ "}")

  	
  | "west" -> 
    let curr_loc = List.assoc current_player w.player in
  	let new_loc_x = fst curr_loc - 1 in
    let new_loc_y = snd curr_loc in
    JMove ("{\"new_x\":" ^ (string_of_int new_loc_x) ^  ", \"new_y\": " ^ 
    (string_of_int new_loc_y) ^ "}")
  	
  | _ -> raise Illegal


(* [interp_move m w] returns a diff list based on a move command m and world w *)
let interp_spell s w: diff =
  (* find the room the player is in*
   * find the spell that they want to use in the inventory (matches incantation) *)
   match (find_item s w) with
   | Some s -> failwith "Unimplemented"
   | None -> raise NotAnItem

(* [interp_move m w] returns a diff list based on a move command m and world w *)
let interp_take t w: diff =
  (* find the room the object is in, find the item they want to take*)
   match (find_item t w) with
   | Some t -> JTake ("{\"Id\":" ^ (string_of_int t) ^ "}")
   | None -> raise NotAnItem


(* [interp_move m w] returns a diff list based on a move command m and world w *)
let interp_drop d w: diff =
  (* find room player is in, find item they want to drop in inventory*)
   match (find_item d w) with
   | Some d -> JTake ("{\"Id\":" ^ (string_of_int d) ^ "}")
   | None -> raise NotAnItem


(* [interpret_command c] returns a command_json list based on a command*)
let interpret_command c (w: world) pid: command_json=
  match c with
  | Move s -> interp_move s w
  | Spell s -> inter_spell s w
  | Quit -> JQuit
  | Take s -> interp_take s w
  | Drop s -> interp_drop s w
  | Look -> JLook
  | Inventory -> JInv
  | Drink s -> interp_drink s w
  | ViewState -> JViewState
  | Help -> JHelp
