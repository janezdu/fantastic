open Controller
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util

type json = Yojson.Basic.json
type diff = Controller.diff

let j =
  {
    "ditems": [
      "((0,0), [{  pid = 1;\n  pname = \"Rebecca\";\n  pdescr = \"Hi\";\n}])",
      "((0,0), [{  pid = 2;\n  pname = \"Chau\";\n  pdescr = \"Hi\";\n}])",
      "((0,0), [{  pid = 3;\n  pname = \"Chau\";\n  pdescr = \"Hi\";\n}])",
      "((0,0), [{  pid = 4;\n  pname = \"Elle\";\n  pdescr = \"Hi\";\n}])"
    ]
  }

(* delete this later *)
type diff = {
  ditems : (room_loc * (diff_item list)) list ;
}


(* [translate_to_json d] returns a json based on diffs *)
let translate_to_json d = failwith "1"

(* [translate_to_diff j] returns diffs based on a json *)
let translate_to_diff j =
  let ditems = j |> member "ditems" |> to_list |> List.map parse_room in


(* [send_json j} sends a json to the servers. Returns unit *)
let send_json j =
  let () =
  let body = Lwt_main.run body in
  print_endline ("Received body\n" ^ body)


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