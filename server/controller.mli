(* identification of each client *)
type clientid
type diff = Model.diff
type json = string
type cmd

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
type serverstate

(* Returns an integer that represents the most up-to-date timestamp *)
val curtime : serverstate -> int

(* returns the diff for a client when it asks for an update *)
val getClientUpdate : serverstate -> clientid -> diff list

(* tries to change the model based on a client's request. Returns true
 * if change was successful, false o/w. *)
val pushClientUpdate: int -> string -> string -> string

(* [translate_to_diff j] returns diffs based on a json *)
val translate_to_diff: json -> string -> int -> diff list
