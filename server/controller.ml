open Model
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


(* ========================== DEBUGGINGGGGGGGGG ================ *)
let debugging = Model.debugging
let pr msg = if debugging then print_endline msg else ignore ()

(* Globals *)
let newid = ref 1000
let winscore = 500
let gameEnded = ref false
let gameWinner = ref ""
let size = 20

(* todo: implement this in translate_to_diff *)
(* type cmd = Move | Use | Take | Drop *)

exception IllegalStep of string
(* exception IllegalMove
exception IllegalUse of string
exception IllegalTake
exception IllegalDrop *)

exception EndGame of string
exception WorldFailure of string

let file = (Yojson.Basic.from_file ("worlds/ttworld.json"))
let init_state_var =  init_state file



let state = ref {flatworld = (init_state_var);
                 client_diffs = [(1234, [])];
                 alldiffs = []}

let rec remove_from_list x = function
  | [] -> failwith "No such element"
  | h::t -> if h = x then t else h::(remove_from_list x t)

(* [translate_to_diff j] returns diffs based on a json string [j] and
 * a string [r] that determines what type of cmd is being requested
 * among ["move", "use", "take", "drop"] *)
let translate_to_diff snapshot j r cid =
  pr j;
  let json = j |> Yojson.Basic.from_string in
  let {flatworld; client_diffs} = snapshot in
  let (curx, cury) = List.assoc cid flatworld.players in
  pr ("Old location of player: ("^(string_of_int curx)
                 ^", "^(string_of_int cury)^")");
  let cur_loc = (curx, cury) in
  let cur_room = RoomMap.find (curx, cury) flatworld.rooms in
  let player =
    match flatworld.items |> LibMap.find cid with
    | IPlayer (p) -> p | _ -> raise (IllegalStep "Not a player, cannot move")
  in
  if r = "quit" then begin
    pr ("player "^(string_of_int cid)^" is quitting");
    [ Remove {loc=cur_loc; id=cid; newitem=IVoid};]
  end
  else if r = "move" then begin
    let new_x = json |> member "new_x" |> to_int in
    let new_y = json |> member "new_y" |> to_int in
    if (abs(new_x - curx) + abs(new_y - cury)) <> 1 then
      raise (IllegalStep "Cannot step to non-adjacent.")
    else
    pr "got new_x and new_y from json";
    [ Remove {loc=(curx, cury); id=cid; newitem=IPlayer player};
      Add {loc=(new_x, new_y); id=cid; newitem=IPlayer player}
    ]
  end
  else if r = "use" then begin
    let item_id = json |> member "id" |> to_int in
    pr ("Begin to use "^(string_of_int item_id));
    pr ("Player inv: "^(string_of_inventory player.inventory));
    let target_id = json |> member "target" |> to_int in
    let new_inv = remove_from_list item_id player.inventory in
    pr ("New inventory: "^(string_of_inventory new_inv));

    if (not (LibMap.mem target_id flatworld.items)) then
      raise (IllegalStep ("Bad target: "^(string_of_int target_id)))
    else
    let wrapped_target = flatworld.items |> LibMap.find target_id in
    let wrapped_item = flatworld.items |> LibMap.find item_id in
    match wrapped_item with
    | ISpell spell ->
      begin
        let diffs =
          let targetdiff = match wrapped_target with
            | IPlayer x -> begin
                let newhp = x.hp + spell.effect in
                if newhp <= 0 then
                  let newname = x.name ^ "'s ghost" in
                  Change {loc=cur_loc; id=target_id;
                          newitem=IPlayer{x with name = newname;
                                                 hp = newhp}}
                else
                  Change {loc=cur_loc; id=target_id;
                          newitem=IPlayer{x with hp = newhp}}
              end
            | IAnimal x -> begin
              let newhp = x.hp + spell.effect in
              if newhp <= 0 then
                Remove {loc=cur_loc; id=target_id;
                        newitem=wrapped_target}
              else
                Change {loc=cur_loc; id=target_id;
                        newitem=IAnimal{x with hp = newhp}}
              end
            | IPolice x -> begin
                let newhp = x.hp + spell.effect in
                if newhp <= 0 then
                  Remove {loc=cur_loc; id=target_id;
                          newitem=wrapped_target}
                else
                  Change {loc=cur_loc; id=target_id;
                          newitem=IPolice{x with hp = newhp}}
              end
            | _ -> raise (IllegalStep "Bad target, not a player/ai")
          in
          [targetdiff;
          Change {loc=cur_loc; id=cid;
                  newitem=IPlayer {player with inventory=new_inv}}
          ] in
        diffs
      end
    | IPotion potion ->
        if player.hp + potion.effect <= 0
        then Remove {loc=cur_loc; id=cid;
                     newitem=IPlayer {player with
                                      inventory = new_inv}}::[]
        else Change {loc=cur_loc; id=cid;
                       newitem=IPlayer {player with
                                        hp = player.hp + potion.effect;
                                        inventory=new_inv}}::[]
    | _ -> raise (IllegalStep "not a spell/potion")
  end
  else if r = "take" then begin
    let item_id = json |> member "id" |> to_int in
    if (not (List.mem item_id cur_room.items)) then
      raise (IllegalStep "Not in room")
    else
    let wrapped_item = flatworld.items |> LibMap.find item_id in
    let _ = match wrapped_item with
      | ISpell _| IPotion _ -> true |_ ->
        raise (IllegalStep "not a spell/potion")
    in
    [ Remove {loc=cur_loc; id=item_id; newitem=wrapped_item};
      Change {loc=cur_loc; id=cid;
              newitem=IPlayer {player with
                               inventory=item_id::player.inventory}}
    ]
  end
  else if r = "drop" then begin
    let item_id = json |> member "id" |> to_int in
    let wrapped_item = flatworld.items |> LibMap.find item_id in
    print_endline (string_of_item wrapped_item);
    let _ = match wrapped_item with
      | ISpell _ | IPotion _ -> true |_ ->
        raise (IllegalStep "not a spell/potion") in
    let _ = remove_from_list item_id player.inventory in
    let new_inv = remove_from_list item_id player.inventory in
    pr ("new inv"^(string_of_inventory new_inv));
    [
      Add {loc=cur_loc; id=item_id; newitem=wrapped_item};
      Change {loc=cur_loc; id=cid;
              newitem=IPlayer {player with inventory=new_inv}};

    ]
  end
  else
    []

let rec remove x l = match l with
  | [] -> failwith "No such element"
  | h::t -> if h = x then t
    else h::(remove x t)

(* [translate_to_json d] returns a json based on diffs *)
let translate_to_single_json diff =

  let otype newitem = match newitem with
    | IPlayer _ -> "player"
    | IAnimal _ -> "animal"
    | IPolice _ -> "police"
    | IPotion _ -> "potion"
    | ISpell _ -> "spell"
    | IVoid -> "void"
  in
  let new_item_json newitem = match newitem with
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
    | ISpell s ->
      `Assoc [
        ("id", `Int s.id);
      ]
    | IPotion s->
      `Assoc [
        ("id", `Int s.id);
      ]
    | IVoid ->
      `Assoc [
        ("id", `Int (-1));
      ]
  in

  match diff with
  | Add {loc; id; newitem} ->
    `Assoc [
      ("difftype", `String "add");
      ("roomx", `Int (fst loc));
      ("roomy", `Int (snd loc));
      ("objecttype", `String (otype newitem));
      ("id", `Int id);
      ("item", new_item_json newitem)
    ]
  | Remove {loc; id; newitem} ->
    `Assoc [
      ("difftype", `String "remove");
      ("roomx", `Int (fst loc));
      ("roomy", `Int (snd loc));
      ("objecttype", `String (otype newitem));
      ("id", `Int id)
    ]
  | Change {loc; id; newitem} ->
      `Assoc [
      ("difftype", `String "change");
      ("roomx", `Int (fst loc));
      ("roomy", `Int (snd loc));
      ("objecttype", `String (otype newitem));
      ("id", `Int id);
      ("item", new_item_json newitem)
    ]

(* [translate_to_json d] returns a json based on diffs *)
let translate_to_json difflist =
  let diffs_json =
    let diffs = `List (List.map
                         (fun x -> translate_to_single_json x) difflist) in
    `Assoc [("diffs", diffs)]
  in
  Yojson.Basic.to_string diffs_json
(* returns the diff for a client when it asks for an update *)
let getClientUpdate cid =
  try
    let snapshot = !state in
    (* print_endline ("Client diffs returned: "^
                   (string_of_difflist snapshot.client_diffs)); *)
    let diffs_to_apply = List.assoc cid snapshot.client_diffs in
    let newdiffs = (cid, [])::(List.remove_assoc cid snapshot.client_diffs) in
    let newstate = {snapshot with client_diffs = newdiffs} in
    state := newstate;
    translate_to_json diffs_to_apply
  with
  | _ -> raise (IllegalStep "Bad clientid")

(* This method looks at the cmd and decides if there are any reactions the
 * world will make. For example, if the user attack an animal, this method
 * will create the world 1 time step later, after the beast attacks back.
 *
 * This is only called inside pushClientUpdate, so the world
 * really does only *react* to things that users do. *)
let react oldstate newstate (cmd:string) cmdtype cid =
  let rec contains x l = match l with [] -> false
          | h::t -> if h = x then true else contains x t
  in
  let randomize state i =
    if List.length state.alldiffs = 0 then i - 1
    else
      let d = match List.hd state.alldiffs with
        | Add d -> 1
        | Remove d -> 2
        | Change d -> 3
      in
      let id = (cid * (Random.int i) + d) mod i in
      id
  in
  let spawn_item state =
    let {flatworld;client_diffs;alldiffs} = state in
    let r = randomize oldstate 3 in
    if r < 3 then begin
      let rand_loc = (randomize oldstate size , randomize oldstate 2) in
      let item_id = (randomize oldstate 24) + 1 in
        pr ("spawning "^(string_of_int item_id)^" at ("^(string_of_int (fst rand_loc))^","^(string_of_int (snd rand_loc))^")");
      let item = flatworld.items |> LibMap.find item_id in
      let old_room = flatworld.rooms |> RoomMap.find rand_loc in
      let new_room = {old_room with items = item_id::old_room.items} in
      let new_room_map = flatworld.rooms |> RoomMap.add rand_loc new_room in
      let diff = Add {loc=rand_loc;id=item_id;newitem=item} in

      let new_client_diffs =
        List.map (fun (id,diffs) -> (id, diff::diffs)) client_diffs
      in
      {flatworld={flatworld with rooms=new_room_map};
       client_diffs=new_client_diffs;
       alldiffs=diff::alldiffs
      } end
    else state
  in
  let scoring state = begin
    pr "inside scoring";
    let {flatworld;client_diffs;alldiffs} = state in
    let cur_loc = List.assoc cid flatworld.players in
    let cur_room = flatworld.rooms |> RoomMap.find cur_loc in
    pr ((string_of_int (fst cur_loc))^(string_of_int (snd cur_loc)));
    if cmdtype = "use" then
      let player = match flatworld.items |> LibMap.find cid with
        | IPlayer p -> (pr "got player";p)
        | _ -> raise (IllegalStep ("Not a player, bad cid, tbh "^
                      "how did you even get here")) in
      let command = Yojson.Basic.from_string cmd in
      let target_id = command |> member "target" |> to_int in
      let is_target_alive = contains target_id cur_room.items in
      let new_player =
        IPlayer {player with
                score = player.score + 50 + (if is_target_alive then 0 else 100)} in
      let new_item_map = flatworld.items |> LibMap.add cid new_player in
      let diff = Change {loc=cur_loc;id=cid;newitem=new_player} in
      let new_client_diffs =
        List.map (fun (id,diffs) -> (id, diff::diffs)) client_diffs
      in
      {flatworld={flatworld with items=new_item_map};
       client_diffs=new_client_diffs;
       alldiffs=diff::alldiffs
      }
    else state end
  in
  let chasing state =
    pr "inside chasing";
    let {flatworld;client_diffs;alldiffs} = state in
    let old_loc = List.assoc cid oldstate.flatworld.players in
    let new_loc = List.assoc cid flatworld.players in
    let old_room = flatworld.rooms |> RoomMap.find old_loc in
    let new_room = flatworld.rooms |> RoomMap.find new_loc in
    let is_police id =
      let item = flatworld.items |> LibMap.find id in
      match item with
      | IPolice ai -> true
      | _ -> false
    in
    try
      let police_id = List.find (fun x -> is_police x) old_room.items in
      let police = flatworld.items |> LibMap.find police_id in
      let new_room_map = flatworld.rooms
                         |> RoomMap.add old_loc
                           {old_room with items=(remove police_id old_room.items)}
                         |> RoomMap.add new_loc
                           {new_room with items=police_id::new_room.items}
      in
      let diff1 = Remove {loc=old_loc;id=police_id;newitem=police} in
      let diff2 = Add {loc=new_loc;id=police_id;newitem=police} in
      let new_client_diffs =
        List.map (fun (id,diffs) -> (id, diff1::diff2::diffs)) client_diffs
      in
      {flatworld={flatworld with rooms=new_room_map};
       client_diffs=new_client_diffs;
       alldiffs=diff1::diff2::alldiffs
      }
    with _ -> state
  in
  let automatic_attack state =
    pr "inside automatic_attack";

    let {flatworld;client_diffs;alldiffs} = state in
    let rec room_locs (i,j) n=
      if j=n then []
      else
        let next =
        if i = n-1 then room_locs (0,j+1) n else room_locs (i+1,j) n in
        (i,j)::next
    in
    let attack st loc =
      let room = flatworld.rooms |> RoomMap.find loc in
      let player_id_list = List.filter
          (fun x -> pr ("id: "^(string_of_int x));
            x >= 1000) room.items in
      let rec print_list = function
      [] -> ()
        | e::l -> print_int e ; print_string " " ; print_list l in
      (* print_endline "List of players:";
      print_list player_id_list;
      print_endline ""; *)
      (* print_endline "end of player_id_list"; *)

      if List.length player_id_list = 0 then st
      else
        let rand = randomize oldstate (List.length player_id_list) in
        let player_id = List.nth player_id_list rand in
        let IPlayer player = flatworld.items |> LibMap.find player_id in
        let beast_attack b =
          let ISpell first_spell = flatworld.items |> LibMap.find (List.hd b.spells) in
          first_spell.effect
        in
        pr "beast_attack";

        let f x = match (flatworld.items |> LibMap.find x) with
                  | IAnimal a -> beast_attack a | _ -> 0
        in
        let sum_spell_beast = List.fold_left (fun a b -> a + (f b)) 0
        (flatworld.rooms |> RoomMap.find loc).items
        in
        if player.hp <= sum_spell_beast
        then begin
          let room = flatworld.rooms |> RoomMap.find loc in
          let new_room_map = flatworld.rooms
                     |> RoomMap.add loc
                       {room with items=(remove player.id room.items)} in
          let diff = Remove {loc=loc;id=player.id;newitem=IPlayer player}
          in
          let new_client_diffs =
            List.map (fun (id,diffs) -> (id, diff::diffs)) client_diffs in
          let new_flat_world =
            {rooms=new_room_map;
            players=List.remove_assoc cid flatworld.players;
            items=flatworld.items}
          in
          {flatworld=new_flat_world;
           client_diffs=new_client_diffs;
           alldiffs=diff::alldiffs
          }
        end
        else begin
          let new_player = IPlayer {player with hp = player.hp - sum_spell_beast} in
          let new_item_map = flatworld.items |> LibMap.add cid new_player in
          let diff = Change {loc=loc;id=cid;newitem=new_player} in
          let new_client_diffs =
            List.map (fun (id,diffs) -> (id, diff::diffs)) client_diffs
          in
          {flatworld={flatworld with items=new_item_map};
           client_diffs=new_client_diffs;
           alldiffs=diff::alldiffs
          }
      end
    in
    pr "----";
    List.fold_left attack state (room_locs (0, 0) size)
  in
  let beast_killing state =
    pr "inside beast_killing";
    try
      let {flatworld;client_diffs;alldiffs} = state in
      let IPlayer player = flatworld.items |> LibMap.find cid in
      let command = Yojson.Basic.from_string cmd in
      let target_id = command |> member "target" |> to_int in
      let target = flatworld.items |> LibMap.find target_id in
      let cur_loc = List.assoc cid flatworld.players in
      let cur_room = flatworld.rooms |> RoomMap.find cur_loc in
      match target with
      | IAnimal _ | IPolice _ | IPlayer _ -> begin
        let is_target_alive = contains target_id cur_room.items in
        if is_target_alive then
            if player.hp <= 20
            then begin
              let new_room_map = flatworld.rooms
                         |> RoomMap.add cur_loc
                           {cur_room with items=(remove cid cur_room.items)} in
              let diff = Remove {loc=cur_loc;id=cid;newitem=IPlayer player}
              in
              let new_client_diffs =
                List.map (fun (id,diffs) -> (id, diff::diffs)) client_diffs in
              let new_flat_world =
                {rooms=new_room_map;
                players=List.remove_assoc cid flatworld.players;
                items=flatworld.items}
              in
              {flatworld=new_flat_world;
               client_diffs=new_client_diffs;
               alldiffs=diff::alldiffs
              }
            end
            else begin
              let new_player = IPlayer {player with hp = player.hp - 20} in
              let new_item_map = flatworld.items |> LibMap.add cid new_player in
              let diff = Change {loc=cur_loc;id=cid;newitem=new_player} in
              let new_client_diffs =
                List.map (fun (id,diffs) -> (id, diff::diffs)) client_diffs
              in
              {flatworld={flatworld with items=new_item_map};
               client_diffs=new_client_diffs;
               alldiffs=diff::alldiffs
              }
            end
        else state
      end
      | _ -> failwith "Not a beast"
    with _ -> state
  in
  newstate |> spawn_item |> scoring |> chasing
   |> automatic_attack
  |> beast_killing


(* tries to change the model based on a client's request.
 * Returns a string that is a jsondiff, i.e. a string formatted with the json
 * schema for diffs*)
let pushClientUpdate cid cmd cmdtype =
  try
    if !gameEnded = true then begin
      raise (EndGame (!gameWinner))
    end
    else
    print_endline (string_of_bool !gameEnded);

    (* Basically we're just pulling the state out of its ref. *)
    let snapshot = !state in
    if not (List.mem_assoc cid snapshot.flatworld.players) then
      raise (WorldFailure "Illegal client id. Aborting post.")
    else
    print_endline ("["^ (string_of_int cid) ^ "] got inside pushClientUpdate");
    print_endline cmdtype;


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

    (* Aplpy react *)
    let afterstate =
      react snapshot {flatworld = newworld;
                      client_diffs = addDiffsToAll;
                      (* This adds the diffs to [alldiffs], which is basically
                         a history of all diffs that have happened in the game
                         so far. This is only here because if a new player joins
                         the game, they need the entire history in diffs. *)
                      alldiffs = diffs@snapshot.alldiffs} cmd cmdtype cid in

    (* If the player has won, don't send back the diffs yet. *)
    let IPlayer player = (LibMap.find cid afterstate.flatworld.items) in
    if player.score > winscore then (
      gameEnded := true;
      print_endline "game ended";
      gameWinner := player.name;
      raise (EndGame (player.name))
    )
    else

    (* [toflush] are the diffs that the client will be getting. *)
    let toflush = List.assoc cid afterstate.client_diffs in
    (* Flush [toflush] from the list of client_diffs. *)
    let flushed = (cid, [])::(List.remove_assoc cid afterstate.client_diffs) in
    state := {afterstate with client_diffs = flushed};
(*
    print_endline ("alldiffs: "^(string_of_difflist [0,afterstate.alldiffs])); *)
    print_libmap afterstate.flatworld.items;


    toflush |> translate_to_json
  with
  | IllegalStep msg -> raise (WorldFailure msg)
  | EndGame winner -> raise (EndGame winner)
  (* | _ -> begin
      (* endWrite (); *)
      raise (WorldFailure ("error applying to world")) end *)


let check_clientid cid = LibMap.mem cid (!state).flatworld.items

let registerUser name =
  try
    let snapshot = !state in

    (* Check if user has already been joined into game.
     * Linear to # of players in game, unfortunately. Can comment out
     * this block *)
    let namelist = List.map (fun id ->
        match (LibMap.find id snapshot.flatworld.items) with
         | IPlayer p -> p.name
         | _ -> failwith "looking for player name of non-player")
        (fst (List.split snapshot.flatworld.players)) in
    if List.mem name namelist then
      raise (WorldFailure ("That username has already been taken!"))
    else
      (* END block that checks for name availablility *)


    let cid = !newid in
    newid := !newid + 1;
    print_endline ("what d'yu know, your cid is "^ (string_of_int cid));
    (* TODO: maybe randomize staring inventory? *)
    let newPlayer = IPlayer {id = cid;
                             name = name;
                             hp = 1000;
                             score = 0;
                             inventory = [1;1;1;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2]} in
    (* Create diffs so that other players can add this player *)
    let diffs = [Add {loc = (0,0); id = cid; newitem = newPlayer}] in

    (* Create a new flatworld where you have added the new player *)
    let newworld = List.fold_left
        (fun a d -> apply_diff d a)
        (snapshot.flatworld) diffs in

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
