open Model
open Concurrent

open Yojson.Basic.Util

(* identification of each client *)
type clientid = int

type serverstate = {
  flatworld : world ;
  client_diffs: (int * diff list) list;
  alldiffs : diff list;
}

type json = string

type diff = Model.diff

let p = print_endline

let newid = ref 1000

(* todo: implement this in translate_to_diff *)
type cmd = Move | Use | Take | Drop

exception IllegalStep of string
(* exception IllegalMove
exception IllegalUse of string
exception IllegalTake
exception IllegalDrop *)

exception WorldFailure of string
let state = ref {flatworld = (init 4);
                 client_diffs = [(1234, [])];
                 alldiffs = []}

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
  print_endline ("("^(string_of_int curx)^", "^(string_of_int cury)^")");
  let cur_loc = (curx, cury) in
  let cur_room = RoomMap.find (curx, cury) flatworld.rooms in
  let IPlayer (player) = flatworld.items |> LibMap.find cid in
  if r = "move" then begin
    let newx = json |> member "newx" |> to_int in
    let newy = json |> member "newy" |> to_int in
    print_endline "got newx and newy from json";
    [
      Remove {loc=(curx, cury); id=cid; newitem=IPlayer player};
      Add {loc=(newx, newy); id=cid; newitem=IPlayer player}
    ]
  end
  else if r = "use" then begin
    try
      let item_id = json |> member "id" |> to_int in
      print_endline (string_of_int item_id);
      let target_id = json |> member "target" |> to_int in
      let new_inv = remove_from_list item_id player.inventory in
      print_endline (string_of_inventory new_inv);

      if (not (LibMap.mem target_id flatworld.items)) then
        begin
          print_endline "attacking bad thing";
          raise (IllegalStep "Not an available target")
        end
      else
      let wrapped_target = flatworld.items |> LibMap.find target_id in
      let wrapped_item = flatworld.items |> LibMap.find item_id in
      match wrapped_item with
      | ISpell spell ->
        begin
          let diff_target =
            let (new_target, target_hp) = match wrapped_target with
              | IPlayer x -> (IPlayer {x with hp = x.hp + spell.effect}, x.hp)
              | IPolice x -> (IPolice {x with hp = x.hp + spell.effect}, x.hp)
              | IAnimal x -> (IAnimal {x with hp = x.hp + spell.effect}, x.hp)
              | _ -> failwith "not a player/ai"
            in
            (* if target_hp + spell.effect <= 0
            then Remove {loc=cur_loc; id=target_id; newitem=wrapped_target}
            else  *)
              Change {loc=cur_loc; id=target_id; newitem=new_target}
          in
          [
            Change {loc=cur_loc; id=cid; newitem=IPlayer {player with inventory=new_inv}};
            diff_target
          ]
        end
      | IPotion potion ->
        begin
          let diff_player =
            if player.hp + potion.effect <= 0
            then Remove {loc=cur_loc; id=cid; newitem=IPlayer player}
            else Change {loc=cur_loc; id=cid;
                         newitem=IPlayer {player with hp = player.hp + potion.effect}}
          in
          [
            Change {loc=cur_loc; id=cid; newitem=IPlayer {player with inventory=new_inv}};
            diff_player
          ]
        end
      | _ -> failwith "not a spell/potion"
    with
    | IllegalStep msg -> raise (IllegalStep msg)
    | _ -> failwith "weird error ?? "
  end
  else if r = "take" then begin
    try
      let item_id = json |> member "id" |> to_int in
      if (not (List.mem item_id cur_room.items)) then
        raise (IllegalStep "Not in room")
      else
      let wrapped_item = flatworld.items |> LibMap.find item_id in
      let _ = match wrapped_item with
        | ISpell _| IPotion _ -> true |_ -> failwith "not a spell/potion"
      in
      [
        Remove {loc=cur_loc; id=item_id; newitem=wrapped_item};
        Change {loc=cur_loc; id=cid;
                newitem=IPlayer {player with inventory=item_id::player.inventory}}
      ]
    with _ -> raise (IllegalStep "Bad take")
  end
  else if r = "drop" then begin
    try
      let item_id = json |> member "id" |> to_int in
      let wrapped_item = flatworld.items |> LibMap.find item_id in
      let _ = match wrapped_item with
        | ISpell _| IPotion _ -> true |_ -> failwith "not a spell/potion"
      in
      let _ = remove_from_list item_id player.inventory in
      let new_inv = remove_from_list item_id player.inventory in
      [
        Change {loc=cur_loc; id=cid; newitem=IPlayer {player with inventory=new_inv}};
        Add {loc=cur_loc; id=item_id; newitem=wrapped_item}
      ]
    with
    | _ -> raise (IllegalStep "Bad drop")
  end
  else
    []


let rec remove x l = match l with
  | [] -> failwith "no such element"
  | h::t -> if h = x then t
    else h::(remove x t)

(* [translate_to_json d] returns a json based on diffs *)
let translate_to_single_json diff =
  let objecttype item = match item with
    | IPlayer _ -> "player"
    | IAnimal _ |IPolice _ -> "ai"
    | IPotion _ | ISpell _ -> "inv_item"
    | _ -> failwith "invalid item"
  in
  match diff with
  | Add {loc; id; newitem} ->
    `Assoc [
      ("difftype", `String "add");
      ("roomx", `Int (fst loc));
      ("roomy", `Int (snd loc));
      ("objecttype", `String (objecttype newitem));
      ("id", `Int id)
    ]
  | Remove {loc; id; newitem} ->
    `Assoc [
      ("difftype", `String "remove");
      ("roomx", `Int (fst loc));
      ("roomy", `Int (snd loc));
      ("objecttype", `String (objecttype newitem));
      ("id", `Int id)
    ]
  | Change {loc; id; newitem} ->
      let otype = objecttype newitem in
      let new_item_json = match newitem with
        | IPlayer player ->
          `Assoc [
          ("id", `Int player.id);
          ("name", `String player.name);
          ("hp", `Int player.hp);
          ("score", `Int player.score);
          ("inventory", `List (List.map (fun i -> `Int i) player.inventory))
          ]
        | IAnimal ai | IPolice ai->
          `Assoc [
          ("id", `Int ai.id);
          ("hp", `Int ai.hp);
          ]
        | _ -> failwith "invalid item type to change"
      in
      `Assoc [
      ("difftype", `String "change");
      ("roomx", `Int (fst loc));
      ("roomy", `Int (snd loc));
      ("objecttype", `String otype);
      (otype, new_item_json)
    ]

(* [translate_to_json d] returns a json based on diffs *)
let translate_to_json difflist =
  let diffs_json =
    `List (List.map (fun x -> translate_to_single_json x) difflist) in
  Yojson.Basic.to_string diffs_json
(* returns the diff for a client when it asks for an update *)
let getClientUpdate cid =
  try
    let snapshot = !state in
    print_endline (string_of_difflist snapshot.client_diffs);
    let diffs_to_apply = List.assoc cid snapshot.client_diffs in
    let newdiffs = (cid, [])::(List.remove_assoc cid snapshot.client_diffs) in
    let newstate = {snapshot with client_diffs = newdiffs} in
    state := newstate;
    translate_to_json diffs_to_apply
  with
  | _ ->
    failwith "illegal client"

(* This method looks at the cmd and decides if there are any reactions the
 * world will make. For example, if the user attack an animal, this method
 * will create the world 1 time step later, after the beast attacks back.
 *
 * This is only called inside pushClientUpdate and registerUser, so the world
 * really does only *react* to things that users do. *)
let react oldstate newstate (cmd:string) cid = newstate

(* tries to change the model based on a client's request.
 * Returns a string that is a jsondiff, i.e. a string formatted with the json
 * schema for diffs*)
let pushClientUpdate cid cmd cmdtype =
  try
    print_endline ("["^ (string_of_int cid) ^ "] got inside pushClientUpdate");
    (* Basically we're just pulling the state out of its ref. *)
    let snapshot = !state in
    (* [diffs] is a list of type diff (add remove change). This is the result
     * of the player's action; if the player moved, this includes only the diffs
     * generated by the move, and none of its consequences like a beast
     * attacking back. *)
    let diffs = (translate_to_diff snapshot cmd cmdtype cid) in

    print_endline (string_of_difflist snapshot.client_diffs);

    (* This creates a new flatworld that with [diffs] applied to it. Again, this
       does not include the consequences like a beast attacking back.    *)
    let newworld = List.fold_left (fun a d -> apply_diff d a)
        (snapshot.flatworld) diffs in

    (* Add the new diffs to every player's stack. *)
    let addDiffsToAll = List.map
        (fun (id,lst) -> (id, diffs@lst)) snapshot.client_diffs in
    print_endline (string_of_difflist addDiffsToAll);

    (* Aplpy react *)
    let afterstate =
      react snapshot {flatworld = newworld;
                      client_diffs = addDiffsToAll;
                      (* This adds the diffs to [alldiffs], which is basically
                         a history of all diffs that have happened in the game
                         so far. This is only here because if a new player joins
                         the game, they need the entire history in diffs. *)
                      alldiffs = diffs@snapshot.alldiffs} cmd cid in
    (* [toflush] are the diffs that the client will be getting. *)
    let toflush = List.assoc cid afterstate.client_diffs in
    (* Flush [toflush] from the list of client_diffs. *)
    let flushed = (cid, [])::(List.remove_assoc cid afterstate.client_diffs) in

    state := {afterstate with client_diffs = flushed};

    print_endline ("alldiffs: "^(string_of_difflist [0,afterstate.alldiffs]));
    print_libmap afterstate.flatworld.items;

    toflush |> translate_to_json
  with
  | IllegalStep msg -> raise (WorldFailure msg)
  | _ -> begin
      (* endWrite (); *)
      raise (WorldFailure ("error applying to world")) end


let check_clientid cid = LibMap.mem cid (!state).flatworld.items

let registerUser name =
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
    (* Create diffs so that other players can add this player *)
    let diffs = [Add {loc = (0,0); id = cid; newitem = newPlayer}] in
    (* Create a new flatworld where you have added the new player *)
    let newworld = List.fold_left (fun a d -> apply_diff d a) (snapshot.flatworld) diffs in
    (* Add new player to client_diffs *)
    let newclientdiffs = (cid, snapshot.alldiffs)::snapshot.client_diffs  in
    let newdiffs = List.map
        (fun (id,lst) -> (id, diffs@lst)) newclientdiffs in
    let afterstate = {flatworld = newworld;
                      client_diffs = newdiffs;
                      alldiffs = diffs@snapshot.alldiffs} in
      (* react {flatworld = newworld;
             client_diffs = newdiffs;
             alldiffs = diffs@snapshot.alldiffs} diffs in *)
    state := afterstate;
    print_endline ("alldiffs: "^(string_of_difflist [0,afterstate.alldiffs]));
    print_libmap afterstate.flatworld.items;
    cid
  with
  | ApplyDiffError msg -> begin
      print_endline "[ERROR] ApplyDiffError with registerUser. Map was: ";
      print_libmap (!state).flatworld.items;
      raise (WorldFailure ("ApplyDiffError: "^ msg))
    end
  | _ -> begin
      raise (WorldFailure ("Stupid things happened when "^
                                           "we tried to register "^ name))
    end
