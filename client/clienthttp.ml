open Controller
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Yojson.Basic

type json = Yojson.Basic.json
type diff = Controller.diff

let client_id = 1234
let action = "move"

let real_j =
  "{
    \"diffs\": [
      {
        \"difftype\": \"add\",
        \"roomx\": 1,
        \"roomy\": 3,
        \"objecttype\": \"player\",
        \"player\": {
          \"id\": 3,
          \"hp\": 90,
          \"score\": 1030,
          \"inv\": [
            1,
            2,
            3,
            3
          ]
        }
      },
      {
        \"difftype\": \"remove\",
        \"roomx\": 1,
        \"roomy\": 2,
        \"objecttype\": \"spell\",
        \"spell\": {
          \"id\": 3
        }
      }
    ]
  }"

let j = real_j |> from_string

(* need to make it iterate the list of diffs later *)
let get_head lst =
  match lst with
  | [] -> failwith "no head"
  | h::t -> h

(* do this after talking to Jane *)
(* now only assume the basic json no list *)
(* [translate_to_json d] returns a json based on diffs *)
let translate_to_json (d:diff) : json =
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

let parse_diff_remove (j:json) roomy roomy objecttype : diff =
  failwith "3"
  (* let pid = j |> member "id" |> to_int in
  Remove ({
    loc = (proomx, proomy);
    newitem = ?? }) *)

let parse_diff_add (j:json) roomy roomy objecttype : diff =
  failwith "3"

let parse_diff_change (j:json) roomy roomy objecttype : diff =
  failwith "3"

(* need to parse each object type differently also the pdifftype differently too! *)
let parse_diff (j: json) : diff =
  let difftype = j |> member "difftype" |> to_string in
  let roomx = j |> member "roomx" |> to_int in
  let roomy = j |> member "roomy" |> to_int in
  let objecttype = j |> member "objecttype" |> to_string in
  match difftype with
  | "add" -> parse_diff_add j roomx roomy objecttype
  | "remove" -> parse_diff_remove j roomx roomy objecttype
  | "change" -> parse_diff_change j roomx roomy objecttype
  | _ -> failwith "wrong diff type"

(* [translate_to_diff j] returns diffs based on a json
 * Precondition : the input [j] is of type json already *)
let translate_to_diff (j:json) : diff list =
  j |> member "diffs" |> to_list |> List.map parse_diff

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

let body jstr query =
  Client.post ~body:([jstr] |> Lwt_stream.of_list |> Cohttp_lwt_body.of_stream)
  (Uri.of_string ("http://0.0.0.0:8000" ^ query)) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  (* below is the function of the client call *)
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt_body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

(* [send_json j} sends a json to the servers. Returns unit *)
let send_json j =
  let jstr = j |> to_string in
  let query = make_query action client_id in
  let body = Lwt_main.run (body jstr query) in
  print_endline ("Received body\n" ^ body)

let () =
  send_json j