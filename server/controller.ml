open Model

(* identification of each client *)
type clientid = int

type serverstate = {
  flatworld : world;
  client_diffs: (diff list ref) list;
}

exception IllegalMove

let state = {flatworld = init (); client_diffs = []} in



(* side effects to flatworld and client_diffs *)
let rec step cid diffs = match diffs with
| [] -> ()
| (loc, items)::t -> begin

    ()
  end 



(* returns list of diffs to apply to model
 * throws "illegalmove" error *)
let validate cid state cmd = 
  match cmd with
  | Move (nx, ny) -> 
    begin
      let (ox, oy) = List.assoc cid flatworld.client_locs in

      if (abs (ox - nx) + abs (oy - ny) = 1) 
        && (nx < 50 && nx >= 0 && ny < 50 && ny >= 0)
        then
        [
          ((ox, oy), [Remove cid]); 
          ((nx, ny), [Add cid])
        ]

      else raise IllegalMove

      (* check if new is legal position: consecutive and in world *)
      (* return diff list *)
    end
  | Use id -> 
    begin
      (* check if int is in inventory. *)
    end 
  | Take id -> 
    begin
      (* check if id is in room *)
    end
  | Drop id ->
    begin
      (* check if int is in inventory. *)
    end

(* just checking; no side effects *)


(* returns the most up-to-date timestamp based on the server state *)
let curtime state  = failwith "unimplemented"

(* returns the diff for a client when it asks for an update *)
let getClientUpdate cid = 
  try 
    let diff_ref = List.nth state.client_diffs cid in
    let diff = !diff_ref in
    diff_ref := [];
    diff
  with failwith "illegal client" 

(* tries to change the model based on a client's request. Returns true
 * if change was successful, false o/w. *)
let pushClientUpdate cid cmd = 

  (* cmd is one pre-parsed json command
   * {"type": "move", "origx" "origy" "newx" "newy"} 
    -> Move(old, new, thing)*)

  (* 2. validate the cmd *)

  (* 3. apply the cmd *)



  let isok = checkall state diffs in

  if isok then step state diffs
  else false




