open Controller
open Model
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util

type json = Yojson.Basic.json
(* type diff = Controller.diff *)

let j =
  "{\"player\":1, \"x\": 2, \"y\": 3}"

(* need to make it iterate the list of diffs later *)
let get_head lst =
  match lst with
  | [] -> failwith "no element"
  | h::t -> h

(* now only assume the basic json no list *)
(* [translate_to_json d] returns a json based on diffs *)
let translate_to_json (d:diff) =
  let lst_items = d.ditems in
  let diff = get_head lst_items in
  let loc = fst diff in
  let loc_x = fst loc in
  let loc_y = snd loc in
  let player_id = get_head (snd diff) in
  "{\"player\":" + player_id + ", \"x\": " + loc_x + ", \"y\": " + loc_y +"}"

(* [translate_to_diff j] returns diffs based on a json *)
let translate_to_diff j =
  failwith "3"
  (* let player = j |> member "player" |> to_int in
  let loc_x = j |> member "x" |> to_int in
  let loc_x = j |> member "y" |> to_int in
  {
    ditems = [((loc_x, loc_y), [Add (IDPlayer player)])];
  }
 *)
(* [send_json j} sends a json to the servers. Returns unit *)
let send_json j =
  (* let () =
  let body = Lwt_main.run (body j) in
  print_endline ("Received body\n" ^ body)
 *)
  failwith "3"

(* client_example.ml
 * follow this link on terminal: https://github.com/mirage/ocaml-cohttp *)


(* >>= is "bind" for call back function (async program)
 * note: async != multithread
 * multithread = two separate programs run parallel, independently
 * async = one program, but runs everything at the same time.
 * for functions that takes time -> let other thread to compute and will
 * come back to get the results later *)
let body =
  Client.post ~body:([j] |> Lwt_stream.of_list |> Cohttp_lwt_body.of_stream)
  (Uri.of_string "http://0.0.0.0:8000") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  (* below is the function of the client call *)
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt_body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let () =
  let body = Lwt_main.run body in
  print_endline ("Received body\n" ^ body)