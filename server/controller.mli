open world
open Changer


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
 * it is up to date with [flatworld].
 *  *)
type serverstate = {
    flatworld : world;
    client_diffs: (diff list) list;
}

type client = {
    clientid : clientid
}

(* returns the most up-to-date timestamp based on the server state *)
val curtime : serverstate -> timeid

(* 
 * input diff is a change that needs to be validated; returns a no-change
 * diff if it is invalid*)
val validate : diff -> diff

(* changes the world by one step, where the difference is the diff *)
val step : world -> diff -> world

(* returns the diff for a client when it asks for an update*)
val update : serverstate -> client -> diff


