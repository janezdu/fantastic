open Model
open Clienthttp
open Cli
open Clienthttp
open Yojson.Basic.Util
open Lwt

exception NotAnItem
exception Illegal
exception Dead

type world = Model.world
type command = Cli.command
type diff = Model.diff
type json = Yojson.Basic.json
type diff_json = Clienthttp.diff_json
type current_player_id = int

let dim_y = 2
let dim_x = 2

let client_id = ref (-1)
let username = ref ""
let ip = ref ""
let is_dead = ref false
let curr_w = ref (init 4)

let cmove = "move"
let cdrink = "drink"
let cspell = "spell"
let cquit = "quit"
let ctake = "take"
let cdrop = "drop"
let clook = "look"
let cinv = "inv"
let cinventory = "inventory"
let cview = "view"
let chelp = "help"
let ccheck = "check"
let cupdate = "update"
let cuse = "use"

let welcome_msg = "Welcome back to the magical world of J.K. Rowling" ^
  "\nFantastic Beasts And Where To Find Them: "
let ask_name_msg = "What should I call you?\n"
let game_instruction_msg = "Game instruction goes here\n"
let invalid_move_msg = "Invalid move. Please try again.\n"
let bad_req_msg = "Bad request\n"
let incorrect_client_id_msg = "Incorrect client_id.\n"
let invalid_command_msg = "Invalid command. Please try again.\n"
let trouble_login_msg = "We are having trouble logging in." ^
  "Please check if you have the right version of world\n"
let same_username_msg = "Your username has been used by another player in " ^
  "the game. Please select a new one."
let trouble_connection_msg = "There is a problem with the connection. "^
  "Please check the connection and enter the ip address of the host again\n"
let next_cmd_msg = "what's next?\n"
let room_desc_msg = "Room description: "
let room_loc_msg = "Room location: "
let room_item_msg = "\nIn the room, there are: "
let inv_item_msg = "Your inventory contains: "
let quit_msg = "bye!\n"
let take_msg item = "Congratulations! You've taken " ^ item
let drop_msg item = "You've drop " ^ item ^ "\n"
let move_msg new_loc =
  "You are now in room "^ (string_of_int_tuple new_loc) ^ "\n"
let drink_msg drink hp =
  "You've had " ^ drink ^ ".\n Now your hp is " ^ (string_of_int hp)
let spell_msg spell target =
  "You've used " ^ spell ^ " on " ^ target
let check_msg hp score =
  "HP: " ^ (string_of_int hp) ^ "\nscore: " ^ (string_of_int score) ^ "\n"
let dead_warning_msg = "The dead can't use that command. Too bad!"
let dead_noti_msg = "Oops, you're dead!"

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
      inventory = item |> member "inventory" |> null_to_list;
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
  print_endline "hello";
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
  | JCheck

(* [init_state json] creates the inital world for the game *)
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

(* [init_state json] creates the inital world for the game *)
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
  LibMap.fold (fun k v acc ->
    if (check_match i k v) then Some k else acc) items None

let new_mod n x=
  if n < 0 then ((n mod x)+x) mod x
  else if n >= x then (n mod x)
  else n

(* [interp_move m w] returns a command_json based on a
 * move command m and world w*)
let interp_move (m:string) current_player (w:world): comm_json =
  match (String.lowercase_ascii m) with
  | "north" ->
    let curr_loc = List.assoc current_player w.players in
    let new_loc_x = fst curr_loc in
    let y = snd curr_loc + 1 in
    let new_loc_y = new_mod y dim_y in
    JMove ("{\"new_x\":" ^ (string_of_int new_loc_x) ^  ", \"new_y\": " ^
    (string_of_int new_loc_y) ^ "}")
  | "south" ->
    let curr_loc = List.assoc current_player w.players in
    let new_loc_x = fst curr_loc in
    let y = snd curr_loc - 1 in
    let new_loc_y = new_mod y dim_y in
    JMove ("{\"new_x\":" ^ (string_of_int new_loc_x) ^  ", \"new_y\": " ^
    (string_of_int new_loc_y) ^ "}")
  | "east" ->
    let curr_loc = List.assoc current_player w.players in
    let x = fst curr_loc + 1 in
    let new_loc_x = new_mod x dim_x in
    let new_loc_y = snd curr_loc in
    JMove ("{\"new_x\":" ^ (string_of_int new_loc_x) ^  ", \"new_y\": " ^
    (string_of_int new_loc_y) ^ "}")
  | "west" ->
    let curr_loc = List.assoc current_player w.players in
    let x = fst curr_loc - 1 in
    let new_loc_x = new_mod x dim_x in
    let new_loc_y = snd curr_loc in
    JMove ("{\"new_x\":" ^ (string_of_int new_loc_x) ^  ", \"new_y\": " ^
    (string_of_int new_loc_y) ^ "}")
  | _ -> raise Illegal

(* [interp_move m w] returns a command_json list based on a move
 * command m and world w *)
(* Note : find the room the player is in*
   * find the spell that they want to use in the inventory
   * (matches incantation) *)
let interp_spell s  t (w:world): comm_json =
   match (find_item s w) with
   | Some s ->
     (match (find_item t w) with
     | Some t -> JSpell ("{\"id\":" ^ (string_of_int s) ^", \"target\":"^(string_of_int t)^"}")
     | None -> raise NotAnItem)
   | None -> raise NotAnItem

(* [interp_move m w] returns a command_json list based on a move
 * command m and world w *)
(* Note: find the room the object is in, find the item they want to take*)
let interp_take t (w:world): comm_json =
   match (find_item t w) with
   | Some t -> JTake ("{\"id\":" ^ (string_of_int t) ^ "}")
   | None -> raise NotAnItem

(* [interp_move m w] returns a command_json list based on a move
 * command m and world w *)
(* find room player is in, find item they want to drop in inventory*)
let interp_drop d (w:world): comm_json =
   match (find_item d w) with
   | Some d -> JDrop ("{\"id\":" ^ (string_of_int d) ^ "}")
   | None -> raise NotAnItem

(* [interp_move m w] returns a command_json list based on a move
 * command m and world w *)
(* find room player is in, find item they want to drop in inventory*)
let interp_drink d current_player (w:world): comm_json =
   match (find_item d w) with
   | Some d -> JDrink ("{\"id\":" ^ (string_of_int d) ^ ", \"target\":"^(string_of_int current_player )^"}")
   | None -> raise NotAnItem

(* [interpret_command c] returns a command_json list based on a command*)
let interpret_command (c: string) current_player (w: world) : comm_json=
  match (parse_comm c) with
  | Move s -> interp_move s current_player w
  | Spell (s,t) -> interp_spell s t w
  | Quit -> JQuit
  | Take s -> interp_take s w
  | Drop s -> interp_drop s w
  | Look -> JLook
  | Inventory -> JInv
  | Drink s -> interp_drink s current_player w
  | ViewState -> JViewState
  | Help -> JHelp
  | Check -> JCheck

let rec print_string_list = function
  | [] -> ()
  | h::t -> print_endline h; print_string_list t

let rec print_string_list_with_number = function
  | [] -> ()
  | (s,n)::t ->
    if n = 1 then
      (print_endline s;
      print_string_list_with_number t)
    else
      (print_endline (s ^ " x " ^ (string_of_int n));
      print_string_list_with_number t)

let get_item_name_by_id lib id =
  match LibMap.find id lib with
  | IPlayer x -> x.name
  | IAnimal x -> x.name
  | IPolice x -> x.name
  | ISpell x -> x.incant
  | IPotion x -> x.name
  | IVoid -> ""

let get_item_name_and_hp_by_id lib id =
  match LibMap.find id lib with
  | IPlayer x -> x.name ^ " (hp = " ^ (string_of_int x.hp) ^ ")"
  | IAnimal x -> x.name ^ " (hp = " ^ (string_of_int x.hp) ^ ")"
  | IPolice x -> x.name ^ " (hp = " ^ (string_of_int x.hp) ^ ")"
  | _ -> ""

let unwrap_player = function
  | IPlayer x -> x
  | _ -> failwith "invalid command"

let rec get_curr_loc = function
  | [] -> (-1,-1)
  | (id, loc)::t -> if id = !client_id then loc else get_curr_loc t

let rec get_hp id lmap =
  let curr_player_item = LibMap.find id lmap in
  let curr_player = unwrap_player curr_player_item in
  curr_player.hp

let rec elim_dup_helper acc = function
  | [] -> acc
  | h::t ->
    if List.mem h t then elim_dup_helper acc t
    else elim_dup_helper (acc@[h]) t

let elim_dup lst = elim_dup_helper [] lst

let rec fold_dup tbl = function
  | [] -> tbl
  | h::t ->
    if Hashtbl.mem tbl h then
      let n = Hashtbl.find tbl h in
      Hashtbl.add tbl h (n+1);
      fold_dup tbl t
    else
      (Hashtbl.add tbl h 1;
      fold_dup tbl t)

let rec make_key_pair_item_helper tbl acc = function
  | [] -> acc
  | h::t ->
    let v = Hashtbl.find tbl h in
    make_key_pair_item_helper tbl (acc@[(h,v)]) t

let make_key_pair_item tbl lst = make_key_pair_item_helper tbl [] lst

(* print out current room that the player is in *)
let print_room w =
  let loc = get_curr_loc w.players in
  let room = RoomMap.find (loc) w.rooms in
  print_endline (room_loc_msg ^ (string_of_int_tuple loc));
  print_endline (room_desc_msg ^ room.descr);
  print_endline room_item_msg;
  let id_list_hp =
    List.filter (fun x -> x > 99 && x <> !client_id) room.items in
  let player_ai_list =
    List.map (get_item_name_and_hp_by_id w.items) id_list_hp in
  let id_list_no_hp = List.filter (fun x -> x < 100) room.items in
  let item_list_dup =
    List.map (get_item_name_by_id w.items) id_list_no_hp in
  let tbl = fold_dup (Hashtbl.create 10) item_list_dup in
  let item_list_no_dup = elim_dup item_list_dup in
  let key_pair_item = make_key_pair_item tbl item_list_no_dup in
  print_string_list player_ai_list;
  print_string_list_with_number key_pair_item;
  print_endline ""

let print_inv w =
  let p = unwrap_player (LibMap.find !client_id w.items) in
  print_endline inv_item_msg;
  let id_list_hp =
    List.filter (fun x -> x > 99 && x < 1000) p.inventory in
  let player_ai_list =
    List.map (get_item_name_and_hp_by_id w.items) id_list_hp in
  let inv_list_dup = List.map (get_item_name_by_id w.items) p.inventory in
  let tbl = fold_dup (Hashtbl.create 10) inv_list_dup in
  let inv_list_no_dup = elim_dup inv_list_dup in
  let key_pair_item = make_key_pair_item tbl inv_list_no_dup in
  print_string_list player_ai_list;
  print_string_list_with_number key_pair_item;
  print_endline ""

let print_help () =
  print_endline game_instruction_msg

let print_check current_player w =
  let player = LibMap.find current_player w.items in
  match player with
  | IPlayer p ->
    print_endline (check_msg p.hp p.score)
  | _ -> failwith "not a player"

(* debugging *)
let get_check current_player w =
  let player = LibMap.find current_player w.items in
  match player with
  | IPlayer p ->
    "hp: " ^ (string_of_int p.hp) ^ " score: " ^ (string_of_int p.score)
  | _ -> failwith "not a player"

(************************** update world **************************************)

(* keep requesting until it's approved then apply diffs to the world *)
let rec request_and_update_world (w: world) : world Lwt.t =
  send_get_request !ip cupdate !client_id >>= fun (code, body) ->
  if code = 200 then
    body >>= fun x ->
    (* debugging *)
    (* print_endline ("diff: "^x); *)
    translate_to_diff x |> apply_diff_list w |> return
  else request_and_update_world w

(************************** eval command **************************************)

(* [do_command comm current_player world] calls a post or get request
 * based on [comm] for command that needs to update the world.
 * For commands that don't, pulls infos from the current world state.
 * Returns a tuple of status code and body Lwt.t *)
let do_command comm current_player w : (int * string Lwt.t) Lwt.t =
  request_and_update_world w >>= fun curr_world ->
  match interpret_command comm current_player curr_world with
  | JMove x -> send_post_request !ip x cmove current_player
  | JDrink x -> send_post_request !ip x cuse current_player
  | JSpell x ->
    (print_endline ("spell " ^ x);
    send_post_request !ip x cuse current_player)
  | JQuit -> send_get_request !ip cquit current_player
  | JTake x -> send_post_request !ip x ctake current_player
  | JDrop x -> send_post_request !ip x cdrop current_player
  | JLook -> print_room curr_world; return ((-1, return ""))
  | JInv -> print_inv curr_world; return ((-1, return ""))
  | JViewState -> print_room curr_world; return ((-1, return ""))
  | JHelp -> (print_help (); return ((-1, return "")))
  | JCheck -> (print_check current_player w; return (-1, return ""))

let do_command_dead comm current_player w : (int * string Lwt.t) Lwt.t =
  request_and_update_world w >>= fun curr_world ->
  match interpret_command comm current_player curr_world with
  | JMove x -> send_post_request !ip x cmove current_player
  | JQuit -> send_get_request !ip cquit current_player
  | JDrop x -> send_post_request !ip x cdrop current_player
  | JLook -> print_room curr_world; return ((-1, return ""))
  | JInv -> print_inv curr_world; return ((-1, return ""))
  | JViewState -> print_room curr_world; return ((-1, return ""))
  | JHelp -> (print_help (); return ((-1, return "")))
  | JCheck -> (print_check current_player w; return (-1, return ""))
  | _ -> raise Dead

(********************************** repl **************************************)

(*  localhost:8000/login?username=chau *)
(* request client_id from server. ?? maybe i need to check other resp code *)
let update_client_id_helper callback name =
  send_login_request !ip name >>= fun (code, body) ->
  match code with
  | 200 -> body >>= fun x -> translate_to_client_id x; return ()
  | 418 -> (print_endline same_username_msg; callback (); return ())
  | _ -> (print_endline (trouble_login_msg); callback (); return ())

let update_client_id callback name =
  ignore (update_client_id_helper callback name)

let get_verb_from_cmd c =
  try
    let c_trim = String.trim c in
    let space_idx = String.index c_trim ' ' in
    let verb =
      String.sub c_trim (0) (space_idx) in
    String.trim verb
  with
  | _ -> String.trim c

let get_obj_from_cmd c =
  try
    let c_trim = String.trim c in
    let space_idx = String.index c_trim ' ' in
    let verb =
      String.sub c_trim (space_idx + 1) (String.length c - space_idx -1) in
    String.trim verb
  with
  | _ -> String.trim c

(* requires: there's a [,] in [c] *)
let get_spell_from c =
  let obj = get_obj_from_cmd c in
  let comma_idx = String.index obj ',' in
  let spell =
    String.sub obj 0 (comma_idx -1) in
  String.trim spell

let get_target_from c =
  let obj = get_obj_from_cmd c in
  let comma_idx = String.index obj ',' in
  let target =
    String.sub obj (comma_idx + 1) (String.length obj - comma_idx -1) in
  String.trim target

let check_life w =
  if (get_hp !client_id w) <= 0 then
    (print_endline dead_noti_msg;
    is_dead := true)
  else ()


let rec repl_helper (c: string) (w: world) : world Lwt.t =
  let request =
    if !is_dead then do_command_dead c !client_id w
    else do_command c !client_id w in
  request >>= fun (code, body) ->
  if code = 200 then
    body >>= fun x ->
    (* for debugging *)
    (* print_endline x; *)
  (*   (body >>= fun x -> print_endline x;
    translate_to_diff x |> apply_diff_list w |> return) *)
    match get_verb_from_cmd c with
    | "move" ->
      (body >>= fun x ->
      let new_w = translate_to_diff x |> apply_diff_list w in
      print_endline (move_msg (get_curr_loc new_w.players));
      repl_helper clook new_w)
    | "drink" ->
      (body >>= fun x ->
      let new_w = translate_to_diff x |> apply_diff_list w in
      let drink = get_obj_from_cmd c in
      let new_hp = get_hp !client_id new_w.items in
      print_endline (drink_msg drink new_hp);
      repl_helper ccheck new_w)
    | "spell" ->
      (body >>= fun x ->
      let new_w = translate_to_diff x |> apply_diff_list w in
      let spell = get_spell_from c in
      let target = get_target_from c in
      print_endline (spell_msg spell target);
      return new_w)
    | "quit" ->
      (body >>= fun x ->
      let new_w = translate_to_diff x |> apply_diff_list w in
      print_endline quit_msg; ignore (exit 0);
      return new_w)
    | "take" ->
      (body >>= fun x ->
      let new_w = translate_to_diff x |> apply_diff_list w in
      print_endline (take_msg (get_obj_from_cmd c));
      repl_helper cinv new_w)
    | "drop" ->
      (body >>= fun x ->
      let new_w = translate_to_diff x |> apply_diff_list w in
      print_endline (drop_msg (get_obj_from_cmd c));
      repl_helper cinv new_w)
    | _ -> failwith "not recorded command"
  (* debug *)
  else if code = -1 then return w
  else
    (* debugging *)
    (body >>= fun x -> print_endline x;
    print_int code; return w)

and repl (w: world): world Lwt.t =
  (* debugging *)
(*   print_endline "------------------------------------------------------------";
  print_endline ("rooms: "); print_roommap w.rooms;
  print_endline "------------------------------------------------------------";
  print_endline ("items: "); print_libmap w.items;
  print_endline "------------------------------------------------------------"; *)
  request_and_update_world w >>= fun new_world ->
  curr_w := new_world;
  check_life new_world.items; print_endline next_cmd_msg; print_string "> ";
  let c = String.lowercase_ascii (read_line ()) in
  Lwt.catch (fun () ->
  request_and_update_world new_world >>= repl_helper c >>= repl)
  (incorrect_command_handler w)

and incorrect_command_handler (w: world) = function
  | Dead -> print_endline dead_warning_msg; repl w
  | _ -> print_endline invalid_command_msg; repl w

(******************************* main functions *******************************)

(* Helper function:
 * [cut_file_type file_name] cuts the .filetype out of a file name
 * var cut_file_type : string -> string *)
let cut_file_type file_name =
  try
    let dot_idx = String.rindex file_name '.' in
    String.sub file_name 0 dot_idx
  with
  | _ -> ""

(* Helper for main:
 * [show_welcome_msg file_name st] prints messages before starting the game
 * var show_welcome_msg : string -> state -> unit *)
let show_welcome_msg file_name st =
  print_string (welcome_msg);
  print_endline (cut_file_type file_name);
  print_endline "";
  do_command clook !client_id st

let rec register_client () =
  username := (read_line ());
  update_client_id register_client !username

let start_chain (file_name: string) (w: world) =
  print_endline "start chain";
  return (register_client ()) >>= fun () ->
  (* (* debug starts *)
  print_endline ("iddddd = " ^ string_of_int (!client_id));
  (* debug ends *) *)
  request_and_update_world w >>= fun new_world ->
(*   (* debug starts *)
  print_endline ("iddddd2 = " ^ string_of_int (!client_id));
  (* debug ends *) *)
  curr_w := new_world;
  show_welcome_msg file_name new_world |> ignore; repl new_world

(* [main f] is the main entry point from outside this module
 * to load a game from file [f] and start playing it *)
let rec main_ctrl ip_address =
  try
    ip := ip_address;
    (* debugging *)
    print_endline "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
    (* print_endline "Please enter the file of the game you want to load.\n";
    print_string  "> ";
    (* debugging *)
    let file_name = read_line () in *)
    let file_name = "fourrooms.json" in
    let file = (Yojson.Basic.from_file ("worlds/"^file_name)) in
    let init_state_var = init_state file in
    print_endline ask_name_msg;
    print_string "> ";
    (* register_client (); *)
    curr_w := init_state_var;
    Lwt_main.run (start_chain file_name init_state_var)
  with
  | Sys_error explanation ->
    (print_endline explanation;
    print_string "\n> ";
    main_ctrl (read_line ()))
  | Unix.Unix_error _ ->
    (print_endline (trouble_connection_msg);
    print_string "> ";
    main_ctrl (read_line ()))
  | _ -> (print_endline (trouble_connection_msg);
    print_string "> ";
    main_ctrl (read_line ()))

(******************************** shell debug *********************************)
(******************************** shell debug *********************************)
(******************************** shell debug *********************************)
(******************************** shell debug *********************************)

(*
 * shell.ml
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(* A mini shell *)

open CamomileLibraryDyn.Camomile
open React
open LTerm_style
open LTerm_text
open LTerm_geom

(* +-----------------------------------------------------------------+
   | Prompt creation                                                 |
   +-----------------------------------------------------------------+ *)

(* The function [make_prompt] creates the prompt. Parameters are:

   - size: the current size of the terminal.
   - exit_code: the exit code of the last executed command.
   - time: the current time. *)
let make_prompt size exit_code time =
  let tm = Unix.localtime time in
  let code = string_of_int exit_code in

  (* Replace the home directory by "~" in the current path. *)
  let path = Sys.getcwd () in
  let path =
    try
      let home = Sys.getenv "HOME" in
      if Zed_utf8.starts_with path home then
        Zed_utf8.replace path 0 (Zed_utf8.length home) "~"
      else
        path
    with Not_found ->
      path
  in

  (* Shorten the path if it is too large for the size of the
     terminal. *)
  let path_len = Zed_utf8.length path in
  let size_for_path = size.cols - 24 - Zed_utf8.length code in
  let path =
    if path_len > size_for_path then
      if size_for_path >= 2 then
        ".." ^ Zed_utf8.after path (path_len - size_for_path + 2)
      else
        path
    else
      path
  in

  eval [
    B_bold true;

    B_fg lcyan;
    S"─( ";
    B_fg lmagenta; S(Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec); E_fg;
    S" )─< ";
    B_fg lyellow; S path; E_fg;
    S" >─";
    S(Zed_utf8.make
        (size.cols - 24 - Zed_utf8.length code - Zed_utf8.length path)
        (UChar.of_int 0x2500));
    S"[ ";
    B_fg(if exit_code = 0 then lwhite else lred); S code; E_fg;
    S" ]─";
    E_fg;
    S"\n";

    B_fg lred; S(try Sys.getenv "USER" with Not_found -> ""); E_fg;
    B_fg lgreen; S"@"; E_fg;
    B_fg lblue; S(Unix.gethostname ()); E_fg;
    B_fg lgreen; S" $ "; E_fg;

    E_bold;
  ]

(* +-----------------------------------------------------------------+
   | Listing binaries of the path for completion                     |
   +-----------------------------------------------------------------+ *)
(*
module String_set = Set.Make(String)

let colon_re = Str.regexp ":"
let get_paths () =
  try
    Str.split colon_re (Sys.getenv "PATH")
  with Not_found ->
    []

(* Get the set of all binaries with a name starting with [prefix]. *)
let get_binaries () =
  Lwt_list.fold_left_s
    (fun set dir ->
       Lwt.catch (fun () ->
         Lwt_stream.fold
           (fun file set ->
              if file <> "." && file <> ".." then
                String_set.add file set
              else
                set)
           (Lwt_unix.files_of_directory dir)
           set)
         (function
           | Unix.Unix_error _ -> return set
           | exn -> Lwt.fail exn))
    String_set.empty
    (get_paths ())
  >|= String_set.elements *)

(* +-----------------------------------------------------------------+
   | Customization of the read-line engine                           |
   +-----------------------------------------------------------------+ *)

(* ! put hp, score, and inventory here *)
(* Signal updated every second with the current time. *)
let time =
  let time, set_time = S.create (Unix.time ()) in
  (* Update the time every second. *)
  ignore (Lwt_engine.on_timer 1.0 true (fun _ -> set_time (Unix.time ())));
  time

class read_line ~term ~history ~exit_code ~binaries = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method completion =
    let prefix  = Zed_rope.to_string self#input_prev in
    let binaries = List.filter (fun file -> Zed_utf8.starts_with file prefix) binaries in
    self#set_completion 0 (List.map (fun file -> (file, " ")) binaries)

  initializer
    self#set_prompt (S.l2 (fun size time -> make_prompt size exit_code time) self#size time)
end

(* +-----------------------------------------------------------------+
   | manage command                                                  |
   +-----------------------------------------------------------------+ *)

(* +-----------------------------------------------------------------+
   | Main loop                                                       |
   +-----------------------------------------------------------------+ *)

let rec loop term history exit_code w =
  request_and_update_world w
  >>= fun new_world ->
  (* curr_w := new_world; *)
  check_life new_world.items;
  (* get_binaries () *)
  (* return ([])
  >>= fun binaries -> *)
  Lwt.catch (fun () ->
    (new read_line ~term ~history:(LTerm_history.contents history)
      ~exit_code ~binaries)#run
    >|= fun command -> (new_world, Some command))
    (function
      | Sys.Break -> return (new_world, None)
      | exn -> Lwt.fail exn)
  >>= function
  | new_world, Some command ->
    Lwt.catch (fun () -> repl_helper command new_world)
      (function
        | Unix.Unix_error (Unix.ENOENT, _, _) ->
          LTerm.fprintls term (eval [B_fg lred; S "command not found"])
          >>= fun () ->
          Lwt.return (Unix.WEXITED 127)
        | exn -> Lwt.fail exn)
    (* >>= fun status ->
    LTerm_history.add history command;
    loop
      term
      history
      (match status with
       | Unix.WEXITED code -> code
       | Unix.WSIGNALED code -> code
       | Unix.WSTOPPED code -> code) *)
  | None ->
    loop term history 130

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let request_and_update_world_for_main term w =
  (term, request_and_update_world w)

let main () =
  LTerm_inputrc.load ()
  >>= fun () ->
  Lwt.catch
    (fun () ->
       Lazy.force LTerm.stdout
       >>= fun term ->
       (print_string
         "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
       print_endline "Please enter the ip address of the server.\n";
       print_string  "> ";
       let ip_address = read_line () in
       ip := ip_address;
       print_endline "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
       let init_state_var = init 4 in
       print_endline ask_name_msg;
       print_string "> ";
       curr_w := init_state_var;
       register_client ();
       request_and_update_world_for_main term w >>= fun (term, new_world) ->
       curr_w := new_world;
       show_welcome_msg file_name new_world |> ignore;
       loop term (LTerm_history.create []) 0) new_world
    (function
      | LTerm_read_line.Interrupt -> Lwt.return ()
      | exn -> Lwt.fail exn)

let () = Lwt_main.run (main ())
