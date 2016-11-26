open Model
open Concurrent

open Yojson.Basic.Util

(* identification of each client *)
type clientid = int

type serverstate = {
  flatworld : world ;
  client_diffs: (int * diff list) list;
}

type json = string

type diff = Model.diff

let p = print_endline

let newid = ref 1000

(* todo: implement this in translate_to_diff *)
type cmd = Move | Use | Take | Drop

exception IllegalStep of string

exception WorldFailure of string
let state = ref {flatworld = (init 4); client_diffs = []}

let rec remove_from_list x = function
  | [] -> failwith "invalid"
  | h::t -> if h = x then t else h::(remove_from_list x t)

(* [translate_to_diff j] returns diffs based on a json string [j] and
 * a string [r] that determines what type of cmd is being requested
 * among ["move", "use", "take", "drop"] *)
let translate_to_diff snapshot j r cid =
  let json = j |> Yojson.Basic.from_string in
  let {flatworld; client_diffs} = snapshot in
  (* let flatworld = !fw in *)
  let (curx, cury) = List.assoc cid flatworld.players in
  print_endline (string_of_int curx);
  print_endline (string_of_int cury);
  let cur_loc = (curx, cury) in
  let cur_room = RoomMap.find (curx, cury) flatworld.rooms in
  print_endline "got past most declarations";
  let IPlayer (player) = flatworld.items |> LibMap.find cid in
  if r = "move" then begin
    print_endline "creating diff: inside move";
    let newx = json |> member "newx" |> to_int in
    let newy = json |> member "newy" |> to_int in
    [
      Remove {loc=(curx, cury); id=cid; newitem=IPlayer player};
      Add {loc=(newx, newy); id=cid; newitem=IPlayer player}
    ]
  end
  else if r = "use" then begin
    try
      let item_id = json |> member "id" |> to_int in
      let target_id = json |> member "target" |> to_int in
      let new_inv = remove_from_list item_id player.inventory in
      let _ = remove_from_list target_id player.inventory in
      let wrapped_target = flatworld.items |> LibMap.find target_id in
      let (new_target, target_hp) = match wrapped_target with
        | IPlayer x -> (IPlayer {x with hp = x.hp + item.effect}, x.hp)
        | IPolice x -> (IPolice {x with hp = x.hp + item.effect}, x.hp)
        | IAnimal x -> (IAnimal {x with hp = x.hp + item.effect}, x.hp)
        | _ -> failwith "not a player/ai"
      in
      let wrapped_item = flatworld.items |> find item_id in
      match wrapped_item with
      | ISpell x ->
        begin
          let diff_target =
            if target_hp + x.effect <= 0
            then Remove {loc=cur_loc; id=target_id; newitem=wrapped_target}
            else Change {loc=cur_loc; id=target_id;
                         newitem=new_target}
          in
          [
            Change {loc=cur_loc; id=cid; newitem=IPlayer {player with inventory=new_inv}};
            diff_target
          ]
        end
      | IPotion x ->
        begin
          let diff_player =
            if player.hp + x.effect <= 0
            then Remove {loc=cur_loc; id=cid; newitem=IPlayer player}
            else Change {loc=cur_loc; id=cid;
                         newitem=Iplayer {player with hp = player.hp + x.effect}}
          in
          [
            Change {loc=cur_loc; id=cid; newitem=IPlayer {player with inventory=new_inv}};
            diff_player
          ]
        end
    | _ -> failwith "not a spell/potion"
    with _ -> raise (IllegalStep "Can't use an item not in inventory")
end
else if r = "take" then begin
  try
    let item_id = json |> member "id" |> to_int in
    let _ = remove_from_list item_id cur_room.items in
    let wrapped_item = flatworld.items |> find item_id in
    let valid_item = match wrapped_item with
      | ISpell x| IPotion x -> true |_ -> failwith "not a spell/potion"
    in
    [
      Remove {loc=cur_loc; id=item_id; newitem=wrapped_item};
      Change {loc=cur_loc; id=cid;
              newitem=IPlayer {player with inventory=item_id::player.inventory}}
    ]
  with _ -> raise (IllegalStep "Illegal to take item not in room")
end
else if r = "drop" then begin
  try
    let item_id = json |> member "id" |> to_int in
    let wrapped_item = flatworld.items |> find item_id in
    let valid_item = match wrapped_item with
      | ISpell x| IPotion x -> true |_ -> failwith "not a spell/potion"
    in
    let _ = remove_from_list item_id player.inventory in
    let new_inv = remove_from_list item_id player.inventory in
    [
      Change {loc=cur_loc; id=cid; newitem=IPlayer {player with inventory=new_inv}};
      Add {loc=cur_loc; id=item_id; newitem=wrapped_item}
    ]
  with
  | _ -> raise (IllegalStep "Can't drop item not in inv")
end
else
  []

let rec remove x l = match l with
  | [] -> failwith "no such element"
  | h::t -> if h = x then t
    else h::(remove x t)

(* [translate_to_json d] returns a json based on diffs *)
let translate_to_json difflist =
  "{\r\n  \"diffs\": [\r\n    {\r\n      \"difftype\": \"add\",\r\n      \"roomx\": 2,\r\n      \"roomy\": 1,\r\n      \"objecttype\": \"player\",\r\n      \"id\": 1234, \r\n      \"item\": {\r\n\t   \"name\" : \"rebecca\"\r\n        \"id\": 1234,\r\n        \"hp\": 90,\r\n        \"inv\": [\r\n          1,\r\n          2,\r\n          3,\r\n          3,\r\n        ]\r\n      }\r\n    },\r\n    {\r\n      \"difftype\": \"remove\",\r\n      \"roomx\": 1,\r\n      \"roomy\": 2,\r\n      \"objecttype\": \"player\",\r\n      \"id\": 1234\r\n    }\r\n  ]\r\n}"

(* returns the diff for a client when it asks for an update *)
let getClientUpdate cid =
  (* beginRead (); *)
  try
    let snapshot = !state in
    let diffs_to_apply = List.assoc cid snapshot.client_diffs in
    p "here i am";
    let newdiffs = (cid, [])::(List.remove_assoc cid snapshot.client_diffs) in
    let newstate = {snapshot with client_diffs = newdiffs} in
    state := newstate;
    (* endRead (); *)
    translate_to_json diffs_to_apply
  with
  | _ ->
    (* endRead ();  *)
    failwith "illegal client"

(* This method looks at the cmd and decides if there are any reactions the
 * world will make. For example, if the user attack an animal, this method
 * will create the world 1 time step later, after the beast attacks back.
 *
 * This is only called inside pushClientUpdate and registerUser, so the world
 * really does only *react* to things that users do. *)
let react state cmd = state

(* tries to change the model based on a client's request.
 * Returns a string that is a jsondiff, i.e. a string formatted with the json
 * schema for diffs*)
let pushClientUpdate cid cmd cmdtype =
  (* beginWrite (); *)
  try
    print_endline ("["^ (string_of_int cid) ^ "] got inside pushClientUpdate");
    let snapshot = !state in
    print_endline "Beginning items libmap: ";
    print_libmap snapshot.flatworld.items;
    let diffs = (translate_to_diff snapshot cmd cmdtype cid) in
    let newworld = List.fold_left (fun a d -> apply_diff d a)
        (snapshot.flatworld) diffs in
    let addDiffsToAll = List.map
        (fun (id,lst) -> (id, diffs@lst)) snapshot.client_diffs in
    (* Flush current user's diffs *)
    let newdiffs = (cid, [])::(List.remove_assoc cid addDiffsToAll) in
    let afterworld = react newworld cmd in
    state := {flatworld = afterworld; client_diffs = newdiffs};
    (* endWrite (); *)
    print_endline "Snapshot: ";
    print_libmap newworld.items;

    print_endline "World after change: ";
    print_libmap (!state).flatworld.items;
    diffs |> translate_to_json
  with
  | IllegalStep msg -> raise (WorldFailure msg)
  | _ -> begin
      (* endWrite (); *)
      raise (WorldFailure ("error applying to world")) end


let registerUser name =
  (* beginWrite (); *)
  try
    let snapshot = !state in
    let cid = !newid in
    newid := !newid + 1;
    print_endline ("what d'yu know, your cid is "^ (string_of_int cid));
    (* TODO: maybe randomize staring inventory? *)
    let newPlayer = IPlayer {id = cid;
                             name = name;
                             hp = 1000;
                             score = 0;
                             inventory = [1;1;1]} in
    let diffs = [Add {loc = (0,0); id = cid; newitem = newPlayer}] in
    let newworld = List.fold_left (fun a d -> apply_diff d a) (snapshot.flatworld) diffs in
    p "made newworld";
    let newdiffs = List.map
        (fun (id,lst) -> (id, diffs@lst)) snapshot.client_diffs in
    print_endline "made it this far";
    let afterworld = react newworld diffs in
    state := {flatworld = afterworld; client_diffs = newdiffs};
    (* endWrite (); *)
    print_libmap snapshot.flatworld.items;
    cid
  with
  | ApplyDiffError msg -> begin
      (* endWrite (); *)
      print_endline "ApplyDiffError";
      print_libmap (!state).flatworld.items;
      raise (WorldFailure ("ApplyDiffError: "^ msg))
    end
  | _ -> begin
      (* endWrite (); *)
      raise (WorldFailure ("Stupid things happened when "^
                                           "we tried to register "^ name))
    end
