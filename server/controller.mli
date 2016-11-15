open Model
open Changer

(* identification of each client *)
type clientid = int

(* Explanation:
 * [flatworld] is the current, up-to-date world. Its representation does not
 * involve any diffs; that is, accessing info from the record is just an access,
 * no computation involved.
 *
 * [client_diffs] is the diffs that each client has with the server. Each client
 * as a list of diffs; whenever a change is made to flatworld, a diff is added
 * to each client's diff list. Whenever a client successfully updates, its
 * entire diff list is flushed. An empty diff list for a client indicates that
 * it is up to date with [flatworld]. *)
type serverstate = {
  flatworld : world;
  client_diffs: (diff list) list;
}

(* returns the most up-to-date timestamp based on the server state *)
val curtime : serverstate -> timeid

(* returns the diff for a client when it asks for an update *)
val getClientUpdate : serverstate -> clientid -> diff list

(* tries to change the model based on a client's request. Returns true
 * if change was successful, false o/w. *)
val pushClientUpdate: serverstate -> diff -> bool

