open Model
open Cli
open Clienthttp
open Yojson.Basic.Util

type command = Cli.command
type diff = Model.diff
type json = Yojson.Basic.json
type jsonstring = string

(* [translate_to_json d] returns a command json string based on diffs *)
let translate_to_json (c: command) : jsonstring =
  failwith "unimplemented"

(* [null_int] represents null of type int *)
let null_int = fnull_int ()

(* [null_string] represents null of type string *)
let null_string = fnull_string ()

(* [null_list] represents null of type list *)
let null_list = fnull_list ()

(* [null_to i f x] parses [x] by applying it to [f] iff [x] is not `Null.
 * Otherwise returns [i] *)
let null_to i f x =
  match x with
  | `Null -> i
  | _ -> x |> f

(* [null_to_int x] parses [x] to type int iff [x] is not `Null.
 * Otherwise returns null_int *)
let null_to_int = null_to null_int to_int

(* [null_to_string x] parses [x] to type string iff [x] is not `Null.
 * Otherwise returns null_string *)
let null_to_string = null_to null_string to_string

(* [null_to_list x] parses [x] to type int list iff [x] is not `Null.
 * Otherwise returns null_list *)
let null_to_list x =
  match x with
  | `Null -> null_list
  | _ -> x |> to_list |> List.map to_int

(* [create_item item] creates type item based on json string [item].
 * If some fields are missing, it replaces them with null value of that type. *)
let create_item item = function
  | "player" ->
    IPlayer ({
      id = item |> member "id" |> to_int;
      name = item |> member "name" |> null_to_string;
      hp = item |> member "hp" |> null_to_int;
      score = item |> member "score" |> null_to_int;
      inventory = item |> member "inv" |> null_to_list;
    })
  | "spell" ->
    ISpell ({
      id = item |> member "id" |> to_int;
      incant = item |> member "incant" |> null_to_string;
      descr = item |> member "descr" |> null_to_string;
      effect = item |> member "effect" |> null_to_int;
    })
  | "potion" ->
    IPotion ({
      id = item |> member "id" |> to_int;
      name = item |> member "name" |> null_to_string;
      descr = item |> member "descr" |> null_to_string;
      effect = item |> member "effect" |> null_to_int;
    })
  | "animal" ->
    IAnimal ({
      id = item |> member "id" |> to_int;
      name = item |> member "name" |> null_to_string;
      descr = item |> member "descr" |> null_to_string;
      hp = item |> member "hp" |> null_to_int;
      spells = item |> member "spells" |> null_to_list;
    })
  | "police" ->
    IPolice ({
      id = item |> member "id" |> to_int;
      name = item |> member "name" |> null_to_string;
      descr = item |> member "descr" |> null_to_string;
      hp = item |> member "hp" |> null_to_int;
      spells = item |> member "spells" |> null_to_list;
    })
  | _ -> failwith "create wrong type"

(* [parse_diff_remove j roomx roomy objecttype id] parses [j] to diff of
 * remove *)
let parse_diff_remove roomx roomy objecttype id : diff =
  Remove ({loc = (roomx, roomy); id = id; newitem = IVoid})

(* [parse_diff_add j roomx roomy objecttype id] parses [j] to diff of
 * add *)
let parse_diff_add (j:json) roomx roomy objecttype id : diff =
  let item = j |> member "item" in
  let new_item = create_item item objecttype in
  Add ({loc = (roomx, roomy); id = id; newitem = new_item})

(* [parse_diff_change j roomx roomy objecttype id] parses [j] to diff of
 * change *)
let parse_diff_change (j:json) roomx roomy objecttype id : diff =
  let item = j |> member "item" in
  let new_item = create_item item objecttype in
  Change ({loc = (roomx, roomy); id = id; newitem = new_item})

(* [parse_diff j] parses json object to diff *)
let parse_diff (j: json) : diff =
  let difftype = j |> member "difftype" |> to_string in
  let roomx = j |> member "roomx" |> to_int in
  let roomy = j |> member "roomy" |> to_int in
  let objecttype = j |> member "objecttype" |> to_string in
  let id = j |> member "id" |> to_int in
  match difftype with
  | "add" -> parse_diff_add j roomx roomy objecttype id
  | "remove" -> parse_diff_remove roomx roomy objecttype id
  | "change" -> parse_diff_change j roomx roomy objecttype id
  | _ -> failwith "wrong diff type"

(* [translate_to_diff j] returns diffs based on a diff json string *)
let translate_to_diff (j:jsonstring) : diff list =
  j |> Yojson.Basic.from_string |> member "diffs" |> to_list
  |> List.map parse_diff