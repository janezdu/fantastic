open Model
open Clienthttp
open Cli
open Yojson.Basic.Util

type command = Cli.command


type diff = Model.diff
type comm_json =
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



exception NotAnItem
exception Illegal


let current_player_id = 1234

(* [init_state json] creates the inital world for the game *)
let init_state json = 
  let player_id = json |> member "player" |> to_int in
  let player_x = json |> member "x" |> to_int in
  let player_y = json |> member "y" |> to_int in
  let my_player = IPlayer {id = player_id; name = ""; hp = 100; score = 0; inventory = []} in

   
  let my_rooms = RoomMap.(empty |> add (0,0) {descr= "hi"; items = [1]}) in

  let my_players = [(player_id, (player_x, player_y))] in
  let my_items = LibMap.(empty ) in
  {rooms = my_rooms; players = my_players; items = my_items}

let rec remove i lst =
  match lst with 
  | [] -> []
  | h::t -> if h = i then t else h::(remove i t)


(* [update_world d] updates the world based on diff d*)
let update_world (d: diff) (w:world) = Model.apply_diff d w

  	

(* [step dl w] updates a world based on a diff list [dl]*)
let rec step (dl: diff list) (w: world) = 
  match dl with
  | [] -> w
  | h::t -> step t (update_world h w)

let check_match i key v= 
  match v with
  | IPlayer p -> if i = p.name then true else false
  | IAnimal a -> if i = a.name then true else false
  | IPolice a -> if i = a.name then true else false
  | ISpell s -> if i = s.incant then true else false
  | IPotion p -> if i = p.name then true else false
  | IVoid -> false




(* [find_item i w] finds item id based on name in world w*)
let find_item i (w:world)= 
  let items = w.items in
  (*let items_lst = LibMap.bindings items in*)
  LibMap.fold (fun k v acc -> if (check_match i k v) then Some k else acc) items None

  

(* [interp_move m w] returns a command_json based on a move command m and world w*)
let interp_move (m:string) current_player (w:world): comm_json =
  match (String.lowercase_ascii m) with 
  | "north" -> 
    
  	let curr_loc = List.assoc current_player w.players in
  	let new_loc_x = fst curr_loc in
  	let new_loc_y = snd curr_loc + 1 in
  	JMove ("{\"new_x\":" ^ (string_of_int new_loc_x) ^  ", \"new_y\": " ^ 
  	(string_of_int new_loc_y) ^ "}")
  	
  | "south" -> 
    let curr_loc = List.assoc current_player w.players in
  	let new_loc_x = fst curr_loc in
    let new_loc_y = snd curr_loc - 1 in
    JMove ("{\"new_x\":" ^ (string_of_int new_loc_x) ^  ", \"new_y\": " ^ 
    (string_of_int new_loc_y) ^ "}")
  	
  | "east" -> 
    let curr_loc = List.assoc current_player w.players in
  	let new_loc_x = fst curr_loc + 1 in
	  let new_loc_y = snd curr_loc in
	  JMove ("{\"new_x\":" ^ (string_of_int new_loc_x) ^  ", \"new_y\": " ^ 
	  (string_of_int new_loc_y) ^ "}")

  	
  | "west" -> 
    let curr_loc = List.assoc current_player w.players in
  	let new_loc_x = fst curr_loc - 1 in
    let new_loc_y = snd curr_loc in
    JMove ("{\"new_x\":" ^ (string_of_int new_loc_x) ^  ", \"new_y\": " ^ 
    (string_of_int new_loc_y) ^ "}")
  	
  | _ -> raise Illegal


(* [interp_move m w] returns a command_json list based on a move command m and world w *)
let interp_spell s (w:world): comm_json =
  (* find the room the player is in*
   * find the spell that they want to use in the inventory (matches incantation) *)
   match (find_item s w) with
   | Some s -> failwith "Unimplemented"
   | None -> raise NotAnItem

(* [interp_move m w] returns a command_json list based on a move command m and world w *)
let interp_take t (w:world): comm_json =
  (* find the room the object is in, find the item they want to take*)
   match (find_item t w) with
   | Some t -> JTake ("{\"Id\":" ^ (string_of_int t) ^ "}")
   | None -> raise NotAnItem


(* [interp_move m w] returns a command_json list based on a move command m and world w *)
let interp_drop d (w:world): comm_json =
  (* find room player is in, find item they want to drop in inventory*)
   match (find_item d w) with
   | Some d -> JTake ("{\"Id\":" ^ (string_of_int d) ^ "}")
   | None -> raise NotAnItem

(* [interp_move m w] returns a command_json list based on a move command m and world w *)
let interp_drink d (w:world): comm_json =
  (* find room player is in, find item they want to drop in inventory*)
   match (find_item d w) with
   | Some d -> JDrink ("{\"Id\":" ^ (string_of_int d) ^ "}")
   | None -> raise NotAnItem


(* [interpret_command c] returns a command_json list based on a command*)
let interpret_command (c:command) current_player (w: world) : comm_json=
  match c with
  | Move s -> interp_move s current_player w
  | Spell s -> interp_spell s w
  | Quit -> JQuit
  | Take s -> interp_take s w
  | Drop s -> interp_drop s w
  | Look -> JLook
  | Inventory -> JInv
  | Drink s -> interp_drink s w
  | ViewState -> JViewState
  | Help -> JHelp


(*let rec repl current_player (w: world): unit =


let main = *)

let do_command comm current_player world: unit=
  match (interpret_command comm current_player world) with
  | JMove x -> Clienthttp.send_post_request x "move"
  | JDrink x -> Clienthttp.send_post_request x "drink"
  | JSpell -> Clienthttp.send_post_request x "spell"
  | JQuit -> Clienthttp.send_get_request x "quit"
  | JTake -> Clienthttp.send_post_request x "take"
  | JDrop -> Clienthttp.send_post_request x "drop"
  | JLook -> Clienthttp.send_get_request x "look"
  | JInv -> Clienthttp.send_get_request x "inventory"
  | JViewState -> Clienthttp.send_get_request x "view"
  | JHelp -> ()
