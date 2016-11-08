open Model

(* Takes in a diff and a world, and updates the world by applying the changes
 * from the diff *)
val applydiff : world -> diff -> world

(* Update the lists of diffs that each client has with the server;
 * contains all info necessary for clients to request update *)
val updateClientDiffs: diff -> diff list -> diff list
