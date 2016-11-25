open Model

open Yojson.Basic.Util

(* identification of each client *)
type clientid = int

type serverstate = {
  flatworld : world;
  client_diffs: (diff list ref) list;
}

type json = string

type diff = Model.diff

(* todo: implement this in translate_to_diff *)
type cmd = Move | Use | Take | Drop

exception IllegalMove
exception IllegalUse
exception IllegalTake
exception IllegalDrop

exception WorldFailure of string

let state = {flatworld = init (); client_diffs = []}

let rec remove_from_list x = function
  | [] -> failwith "invalid"
  | h::t -> if h = x then t else h::(remove_from_list x t)

(* [translate_to_diff j] returns diffs based on a json string [j] and
 * a string [r] that determines what type of cmd is being requested
 * among ["move", "use", "take", "drop"] *)
let translate_to_diff j r cid =
  let json = j |> Yojson.Basic.from_string in
  let {flatworld; client_diffs} = state in
  let (curx, cury) = List.assoc cid flatworld.players in
  let cur_loc = (curx, cury) in
  let cur_room = RoomMap.find (curx, cury) flatworld.rooms in
  let IPlayer (player) = flatworld.items |> LibMap.find cid in
  if r = "move" then begin
    let newx = json |> member "newx" |> to_int in
    let newy = json |> member "newy" |> to_int in
    [
    Remove {loc=(curx, cury); id=cid; newitem=IVoid};
    Add {loc=(newx, newy); id=cid; newitem=IVoid}
    ]
  end
  else if r = "use" then begin
    try
      let item_id = json |> member "id" |> to_int in
      let target_id = json |> member "target" |> to_int in
      let new_inv = remove_from_list item_id player.inventory in
      let _ = remove_from_list target_id player.inventory in
      let wrapped_target = flatworld.items |> LibMap.find target_id in
      let target = match wrapped_target with
        | IPlayer x -> x
        (* TODO: pattern match properly *)
        (* | IPolice x | IAnimal x -> x *)
        | _ -> failwith "not a player/ai"
      in
      let wrapped_item = flatworld.items |> LibMap.find item_id in
      match wrapped_item with
      | ISpell x ->
        begin
          let diff_target =
            if target.hp + x.effect <= 0
            then Remove {loc=cur_loc; id=target_id; newitem=IVoid}
            else Change {loc=cur_loc; id=target_id;
                newitem = IPlayer {target with hp = target.hp + x.effect}}
          in
          [
            (* TODO: can't always wrap in IPlayer *)
          Change {loc=cur_loc; id=cid; newitem= IPlayer {player with inventory=new_inv}};
          diff_target
          ]
        end
      | IPotion x ->
        begin
          let diff_player =
            if player.hp + x.effect <= 0
            then Remove {loc=cur_loc; id=cid; newitem=IVoid}
            else Change {loc=cur_loc; id=cid;
                newitem= IPlayer {player with hp = player.hp + x.effect}}
          in
          [
          Change {loc=cur_loc; id=cid; newitem= IPlayer {player with inventory=new_inv}};
          diff_player
          ]
        end
      | _ -> failwith "not a spell/potion"
    with _ -> raise IllegalUse
  end
  else if r = "take" then begin
    try
      let item_id = json |> member "id" |> to_int in
      let _ = remove_from_list item_id cur_room.items in
      [
      Remove {loc=cur_loc; id=item_id; newitem=IVoid};
      Change {loc=cur_loc; id=cid;
        newitem = IPlayer {player with inventory=item_id::player.inventory}}
      ]
    with _ -> raise IllegalTake
  end
  else if r = "drop" then begin
    try
      let item_id = json |> member "id" |> to_int in
      let _ = remove_from_list item_id player.inventory in
      let new_inv = remove_from_list item_id player.inventory in
      [
        Change {loc = cur_loc; id = cid;
                newitem = IPlayer{player with inventory = new_inv}};
        Add {loc=cur_loc; id=item_id; newitem=IVoid}
      ]
    with
    | _ -> raise IllegalDrop
  end
  else
    []

(* side effects to flatworld and client_diffs *)
let rec step cid diffs = match diffs with
| [] -> ()
| (loc, items)::t -> begin

    ()
  end

let rec remove x l = match l with
  | [] -> failwith "no such element"
  | h::t -> if h = x then t
            else h::(remove x t)

(* returns the most up-to-date timestamp based on the server state *)
let curtime state  = failwith "unimplemented"

(* returns the diff for a client when it asks for an update *)
let getClientUpdate cid =
  failwith "unimplemented"
  (* try
    let diff_ref = List.nth state.client_diffs cid in
    let diff = !diff_ref in
    diff_ref := [];
    diff
  with _ -> failwith "illegal client"
 *)

(* [translate_to_json d] returns a json based on diffs *)
let translate_to_json difflist =
  "{\r\n  \"diffs\": [\r\n    {\r\n      \"difftype\": \"add\",\r\n      \"roomx\": 2,\r\n      \"roomy\": 1,\r\n      \"objecttype\": \"player\",\r\n      \"id\": 1234, \r\n      \"item\": {\r\n\t   \"name\" : \"rebecca\"\r\n        \"id\": 1234,\r\n        \"hp\": 90,\r\n        \"inv\": [\r\n          1,\r\n          2,\r\n          3,\r\n          3,\r\n        ]\r\n      }\r\n    },\r\n    {\r\n      \"difftype\": \"remove\",\r\n      \"roomx\": 1,\r\n      \"roomy\": 2,\r\n      \"objecttype\": \"player\",\r\n      \"id\": 1234\r\n    }\r\n  ]\r\n}"

(* tries to change the model based on a client's request.
 * Returns a string that is a jsondiff, i.e. a string formatted with the json
 * schema for diffs*)
let pushClientUpdate cid cmd cmdtype =
  try
    let diffs = (translate_to_diff cmd cmdtype cid) in
    let _ = List.fold_left (fun a d -> apply_diff d a) state.flatworld diffs in
    diffs |> translate_to_json
  with
  | _ -> raise (WorldFailure ("error applying to world"))
