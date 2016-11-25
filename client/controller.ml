open Model
open Clienthttp
open Cli
open Clienthttp
open Yojson.Basic.Util
open Lwt

exception NotAnItem
exception Illegal

type command = Cli.command
type diff = Model.diff
type json = Yojson.Basic.json
type diff_json = Clienthttp.diff_json
type current_player_id = int

(* status codes *)
let ok = 200
let forbidden = 403
let conflict = 409
let non_found = 404
let unauthorized = 401

(************************** translate_to_diff *********************************)

(* [null_int] represents null of type int *)
let null_int = fnull_int ()

(* [null_string] represents null of type string *)
let null_string = fnull_string ()

(* [null_list] represents null of type list *)
let null_list = fnull_list ()

(* [null_to i f x] parses [x] by applying it to [f] iff [x] is not `Null.
 * Otherwise returns [i] *)
let null_to i f x =
  match x with
  | `Null -> i
  | _ -> x |> f

(* [null_to_int x] parses [x] to type int iff [x] is not `Null.
 * Otherwise returns null_int *)
let null_to_int = null_to null_int to_int

(* [null_to_string x] parses [x] to type string iff [x] is not `Null.
 * Otherwise returns null_string *)
let null_to_string = null_to null_string to_string

(* [null_to_list x] parses [x] to type int list iff [x] is not `Null.
 * Otherwise returns null_list *)
let null_to_list x =
  match x with
  | `Null -> null_list
  | _ -> x |> to_list |> List.map to_int

(* [create_item item] creates type item based on json string [item].
 * If some fields are missing, it replaces them with null value of that type. *)
let create_item item = function
  | "player" ->
    IPlayer ({
      id = item |> member "id" |> to_int;
      name = item |> member "name" |> null_to_string;
      hp = item |> member "hp" |> null_to_int;
      score = item |> member "score" |> null_to_int;
      inventory = item |> member "inv" |> null_to_list;
    })
  | "spell" ->
    ISpell ({
      id = item |> member "id" |> to_int;
      incant = item |> member "incant" |> null_to_string;
      descr = item |> member "descr" |> null_to_string;
      effect = item |> member "effect" |> null_to_int;
    })
  | "potion" ->
    IPotion ({
      id = item |> member "id" |> to_int;
      name = item |> member "name" |> null_to_string;
      descr = item |> member "descr" |> null_to_string;
      effect = item |> member "effect" |> null_to_int;
    })
  | "animal" ->
    IAnimal ({
      id = item |> member "id" |> to_int;
      name = item |> member "name" |> null_to_string;
      descr = item |> member "descr" |> null_to_string;
      hp = item |> member "hp" |> null_to_int;
      spells = item |> member "spells" |> null_to_list;
    })
  | "police" ->
    IPolice ({
      id = item |> member "id" |> to_int;
      name = item |> member "name" |> null_to_string;
      descr = item |> member "descr" |> null_to_string;
      hp = item |> member "hp" |> null_to_int;
      spells = item |> member "spells" |> null_to_list;
    })
  | _ -> failwith "create wrong type"

(* [parse_diff_remove j roomx roomy objecttype id] parses [j] to diff of
 * remove *)
let parse_diff_remove roomx roomy objecttype id : diff =
  Remove ({loc = (roomx, roomy); id = id; newitem = IVoid})

(* [parse_diff_add j roomx roomy objecttype id] parses [j] to diff of
 * add *)
let parse_diff_add (j:json) roomx roomy objecttype id : diff =
  let item = j |> member "item" in
  let new_item = create_item item objecttype in
  Add ({loc = (roomx, roomy); id = id; newitem = new_item})

(* [parse_diff_change j roomx roomy objecttype id] parses [j] to diff of
 * change *)
let parse_diff_change (j:json) roomx roomy objecttype id : diff =
  let item = j |> member "item" in
  let new_item = create_item item objecttype in
  Change ({loc = (roomx, roomy); id = id; newitem = new_item})

(* [parse_diff j] parses json object to diff *)
let parse_diff (j: json) : diff =
  let difftype = j |> member "difftype" |> to_string in
  let roomx = j |> member "roomx" |> to_int in
  let roomy = j |> member "roomy" |> to_int in
  let objecttype = j |> member "objecttype" |> to_string in
  let id = j |> member "id" |> to_int in
  match difftype with
  | "add" -> parse_diff_add j roomx roomy objecttype id
  | "remove" -> parse_diff_remove roomx roomy objecttype id
  | "change" -> parse_diff_change j roomx roomy objecttype id
  | _ -> failwith "wrong diff type"

(* [translate_to_diff j] returns diffs based on a diff json string *)
let translate_to_diff (j:diff_json) : diff list =
  j |> Yojson.Basic.from_string |> member "diffs" |> to_list
  |> List.map parse_diff

(************************** interpret_command *********************************)

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

let current_player_id = 1234

(* [init_state json] creates the inital world for the game *)
let init_state json =
  let player_id = json |> member "player" |> to_int in
  let player_x = json |> member "x" |> to_int in
  let player_y = json |> member "y" |> to_int in
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
  LibMap.fold (fun k v acc -> if (check_match i k v) then
    Some k else acc) items None

(* [interp_move m w] returns a command_json based on a
 * move command m and world w*)
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

(* [interp_move m w] returns a command_json list based on a move
 * command m and world w *)
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
let interpret_command (c:command) current_player (w: world) : comm_json =
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

(* [do_command comm current_player world] calls a post or get request
 * based on [comm] and returns a tuple of status code and body Lwt.t *)
let do_command comm current_player world =
  match (interpret_command comm current_player world) with
  | JMove x -> send_post_request x "move" current_player_id
  | JDrink x -> send_post_request x "drink" current_player_id
  | JSpell x -> send_post_request x "spell" current_player_id
  | JQuit -> send_get_request "quit" current_player_id
  | JTake x -> send_post_request x"take" current_player_id
  | JDrop x -> send_post_request x "drop" current_player_id
  | JLook -> send_get_request "look" current_player_id
  | JInv -> send_get_request "inventory" current_player_id
  | JViewState -> send_get_request "view" current_player_id
  | JHelp -> (-1, return "")