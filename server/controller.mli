(* identification of each client *)
type diff = Model.diff
type json = string
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

exception WorldFailure of string

(* returns the diff for a client when it asks for an update *)
val getClientUpdate : int -> string

(* tries to change the model based on a client's request. Returns the diff
 * json if *)
val pushClientUpdate: int -> string -> string -> string

(* [registerUser s i] Adds a new user to the game state with the name s
 * and returns a tuple [(cid,j)] of its new clientid and a json to add itself *)
val registerUser : string -> int


val check_clientid : int -> bool
