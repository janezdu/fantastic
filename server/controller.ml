open Model

(* identification of each client *)
type clientid = int
type diff = Model.diff

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

