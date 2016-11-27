open Model
open Clienthttp
open Cli
open Clienthttp
open Yojson.Basic.Util
open Lwt

exception NotAnItem
exception Illegal

type world = Model.world
type command = Cli.command
type diff = Model.diff
type json = Yojson.Basic.json
type diff_json = Clienthttp.diff_json
type current_player_id = int


let client_id = ref (-1)
let username = ref ""

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

(********************** translate_to_client_id ********************************)

let translate_to_client_id j =
  j |> int_of_string |> (:=) client_id

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

(* [init_state json] creates the inital world for the game *)


let add_room room_map room_json = 
  let loc = room_json |> member "loc" in
  let x = loc |> member "x" |> to_int in
  let y = loc |> member "y" |> to_int in
  let items = room_json |> member "items" |> to_list |> List.map to_int in
  let des = room_json |> member "descr" |> to_string in
  let room = {descr = des; items = items} in
  RoomMap.add (x,y) room room_map

let add_item item_map item_json = 
  let id = item_json |> member "id" |> to_int in 
  LibMap.empty

let make_player player_json = 
  let id = player_json |> member "id" |> to_int in
  let x = player_json |> member "x" |> to_int in 
  let y=  player_json |> member "y" |> to_int in
  (id, (x,y))

(* [init_state json] creates the inital world for the game *)
let init_state j =
  let orig_room = RoomMap.empty in
  let orig_item = LibMap.empty in
  let rooms = j |> member "rooms" |> to_list in
  let actual_rooms = List.fold_left add_room orig_room rooms in
  let items = j |> member "items" |> to_list |>
    List.fold_left add_item orig_item in
  let player_lst = j |> member "players" |> to_list |>
    List.map make_player in
  {rooms = actual_rooms; players = player_lst; items = items}
  

    


let rec remove i lst =
  match lst with
  | [] -> []
  | h::t -> if h = i then t else h::(remove i t)

(* [step dl w] updates a world based on a diff list [dl]*)
let rec step (dl: diff list) (w: world) =
  match dl with
  | [] -> w
  | h::t -> step t (apply_diff w h)

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
(* Note : find the room the player is in*
   * find the spell that they want to use in the inventory
   * (matches incantation) *)
let interp_spell s (w:world): comm_json =
   match (find_item s w) with
   | Some s -> failwith "Unimplemented"
   | None -> raise NotAnItem

(* [interp_move m w] returns a command_json list based on a move
 * command m and world w *)
(* Note: find the room the object is in, find the item they want to take*)
let interp_take t (w:world): comm_json =
   match (find_item t w) with
   | Some t -> JTake ("{\"Id\":" ^ (string_of_int t) ^ "}")
   | None -> raise NotAnItem

(* [interp_move m w] returns a command_json list based on a move
 * command m and world w *)
(* find room player is in, find item they want to drop in inventory*)
let interp_drop d (w:world): comm_json =
   match (find_item d w) with
   | Some d -> JTake ("{\"Id\":" ^ (string_of_int d) ^ "}")
   | None -> raise NotAnItem

(* [interp_move m w] returns a command_json list based on a move
 * command m and world w *)
(* find room player is in, find item they want to drop in inventory*)
let interp_drink d (w:world): comm_json =
   match (find_item d w) with
   | Some d -> JDrink ("{\"Id\":" ^ (string_of_int d) ^ "}")
   | None -> raise NotAnItem

(* [interpret_command c] returns a command_json list based on a command*)
let interpret_command (c: string) current_player (w: world) : comm_json=
  match (parse_comm c) with
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

let print_string_list = function
  | [] -> ()
  | h::t -> print_endline h

let get_item_name_by_id lib id =
  match LibMap.find id lib with
  | IPlayer x -> x.name
  | IAnimal x -> x.name
  | IPolice x -> x.name
  | ISpell x -> x.incant
  | IPotion x -> x.name
  | IVoid -> ""

let rec get_curr_loc = function
  | [] -> (-1,-1)
  | (id, loc)::t -> if id = !client_id then loc else get_curr_loc t

let print_room w =
  let loc = get_curr_loc w.players in
  let room = RoomMap.find (loc) w.rooms in
  print_endline ("Room description: " ^ room.descr);
  print_endline "In the room, there are: ";
  print_string_list (List.map (get_item_name_by_id w.items) room.items)

let unwrap_player = function
  | IPlayer x -> x
  | _ -> failwith "invalid command"

let print_inv w =
  let p = unwrap_player (LibMap.find !client_id w.items) in
  print_endline "Your inventory contains: ";
  print_string_list (List.map (get_item_name_by_id w.items) p.inventory)

let print_help () =
  print_endline "Game instruction goes here"

(* [do_command comm current_player world] calls a post or get request
 * based on [comm] for command that needs to update the world.
 * For commands that don't, pulls infos from the current world state.
 * Returns a tuple of status code and body Lwt.t *)
let do_command comm current_player world =
  match interpret_command comm current_player world with
  | JMove x -> send_post_request x "move" current_player
  | JDrink x -> send_post_request x "use" current_player
  | JSpell x -> send_post_request x "use" current_player
  | JQuit -> send_get_request "quit" current_player
  | JTake x -> send_post_request x"take" current_player
  | JDrop x -> send_post_request x "drop" current_player
  | JLook -> print_room world; (-1, return "")
  | JInv -> print_inv world; (-1, return "")
  | JViewState -> print_room world; (-1, return "")
  | JHelp -> print_help (); (-1, return "")

(********************************** repl **************************************)

(*  localhost:8000/login?username=chau *)
(* request client_id from server. ?? maybe i need to check other resp code *)
let rec update_client_id_helper name =
  return (send_login_request name) >>= fun (code, body) ->
  if code = 200 then
    body >>= fun x -> translate_to_client_id x; return ()
  else
    (print_endline ("We are having trouble logging in." ^
    "Please check if you have the right version of world");
    update_client_id_helper name)

let update_client_id name =
  ignore (update_client_id_helper name)

(* keep requesting until it's approved then apply diffs to the world *)
let rec request_and_update_world (w: world) : world Lwt.t =
  return (send_get_request "update" !client_id) >>= fun (code, body) ->
  if code = 200 then
    body >>= fun x -> translate_to_diff x |> apply_diff_list w |> return
  else request_and_update_world w

let rec repl_helper (c: string) (w: world) : world Lwt.t =
  return (do_command c !client_id w) >>= fun (code, body) ->
  match code with
  | 200 -> body >>= fun x -> translate_to_diff x |> apply_diff_list w |> repl
  | 403 -> (print_endline "Invalid move. Please try again."; repl w)
  | 409 -> request_and_update_world w >>= repl_helper c
  | 404 -> (print_endline "Invalid move. Please try again."; repl w)
  | 401 -> (update_client_id !username; repl_helper c w)
  | _ -> request_and_update_world w >>= repl_helper c

and repl (w: world): world Lwt.t =
  let c = String.lowercase_ascii (read_line ()) in
  try repl_helper c w with
  | _ -> (print_endline "Invalid command. Please try again."; repl w)

(* Helper function:
 * [cut_file_type file_name] cuts the .filetype out of a file name
 * var cut_file_type : string -> string *)
let cut_file_type file_name =
  let dot_idx = String.rindex file_name '.' in
  String.sub file_name 0 dot_idx

(* Helper for main:
 * [welcome_msg file_name st] prints messages before starting the game
 * var welcome_msg : string -> state -> unit *)
let welcome_msg file_name st =
  print_endline ("Welcome back to the magical world of J.K. Rowling" ^
    "Fantastic Beasts And Where To Find Them:");
  print_endline (cut_file_type file_name);
  do_command "look" !client_id st

(* [main f] is the main entry point from outside this module
 * to load a game from file [f] and start playing it *)
let rec main file_name =
  try
    let init_state_var = init_state (Yojson.Basic.from_file file_name) in
    print_endline "What's your name?";
    username := (read_line ());
    update_client_id !username;
    ignore (welcome_msg file_name init_state_var);
    Lwt_main.run (repl init_state_var)
  with
  | Sys_error explanation ->
    (print_endline explanation;
    print_string "\n";
    main (read_line ()))