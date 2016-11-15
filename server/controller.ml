open Model

(* identification of each client *)
type clientid = int

(* [diff] represents changes that are made in a player's turn.
 * Invariant: [dplayers] and [ditems] only store players and rooms that change.
 * Steady rooms and players must not be included in a [diff]. *)
type diff = {
  ditems : room_loc * (diff_item list) list option;
}

type serverstate = {
  flatworld : world;
  client_diffs: (diff list) list;
}

(* returns the most up-to-date timestamp based on the server state *)
let curtime state  = failwith "unimplemented"

(* returns the diff for a client when it asks for an update *)
let getClientUpdate state cid = failwith "unimplemented"

(* tries to change the model based on a client's request. Returns true
 * if change was successful, false o/w. *)
let pushClientUpdate state d = failwith "unimplemented"

