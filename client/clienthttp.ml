open Controller
open Model
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util

type json = Yojson.Basic.json
type diff = Controller.diff
type jsonstring = string

(* stuff code *)
let client_id = 1234
let action = "move"
let responded_jstr =
  "{
    \"diffs\": [
      {
        \"difftype\": \"add\",
        \"roomx\": 1,
        \"roomy\": 2,
        \"objecttype\": \"potion\",
        \"id\": 23,
        \"item\": {
          \"id\": 23
        }
      },
      {
        \"difftype\": \"change\",
        \"roomx\": 1,
        \"roomy\": 2,
        \"objecttype\": \"player\",
        \"id\": 1234,
        \"item\": {
          \"id\": 1234,
          \"hp\": 90,
          \"inv\": [
            1,
            2,
            3,
            3,
            4
          ]
        }
      },
      {
        \"difftype\": \"remove\",
        \"roomx\": 1,
        \"roomy\": 2,
        \"id\": 4,
        \"objecttype\": \"spell\"
      }
    ]
  }"

(* [translate_to_json d] returns a command json string based on diffs *)
let translate_to_json (d:diff) : jsonstring =
  failwith "1"
  (* let lst_items = d.ditems in
  let diff = get_head lst_items in
  let loc = fst diff in
  let loc_x = fst loc in
  let loc_y = snd loc in
  let player_id = get_head (snd diff) in
  "{\"player\":" ^ "1" ^  (*need to parse player_id later*)
    ", \"x\": " ^ (string_of_int loc_x) ^ ", \"y\": " ^
   (string_of_int loc_y) ^ "}" |> from_string *)

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

(* need to parse each object type differently also the pdifftype differently too! *)
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










let make_query_helper action cid =
  "/" ^ action ^ "?client_id=" ^ (string_of_int cid)

(* ?? do I need this here? *)
let make_query action cid =
  match action with
  | "move" -> make_query_helper "move" cid
  | "use" -> make_query_helper "use" cid
  | "take" -> make_query_helper "take" cid
  | "drop" -> make_query_helper "drop" cid
  | _ -> failwith "invalid command"

let post_body jstr query callback =
  Client.post ~body:([jstr] |> Lwt_stream.of_list |> Cohttp_lwt_body.of_stream)
  (Uri.of_string ("http://0.0.0.0:8000" ^ query)) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  (* below is the function of the client call *)
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt_body.to_string >|= callback

(* stuff callback *)
let cb = fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

(* [send_json j} sends a json to the servers. Returns unit *)
let send_post_request (j: jsonstring) (action: string) callback =
  let query = make_query action client_id in
  let body = Lwt_main.run (post_body j query callback) in
  print_endline ("Received body\n" ^ body)

let get_body jstr query callback =
  Client.get
  (Uri.of_string ("http://0.0.0.0:8000" ^ query)) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  (* below is the function of the client call *)
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt_body.to_string >|= callback

(* [send_json j} sends a json to the servers. Returns unit *)
let send_get_request (j: jsonstring) (action: string) callback =
  let query = make_query action client_id in
  let body = Lwt_main.run (post_body j query callback) in
  print_endline ("Received body\n" ^ body)

let () =
  send_post_request responded_jstr action cb
