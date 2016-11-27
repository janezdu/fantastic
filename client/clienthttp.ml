
open Lwt
open Cohttp
open Cohttp_lwt_unix

type diff_json = string

let valid_actions =
  ["move"; "use"; "take"; "drop"; "quit"; "update"]

let make_query_helper action cid =
  "/" ^ action ^ "?client_id=" ^ (string_of_int cid)

(* [make_query action cid] is [action]?client_id=[cid] *)
let make_query action cid =
  if List.mem action valid_actions then make_query_helper action cid
  else failwith "invalid action"

(* [make_login_query name] is login?username=[name] *)
let make_login_query name =
  "login?username=" ^ name

let post_body jstr query =
  Client.post ~body:([jstr] |> Lwt_stream.of_list |> Cohttp_lwt_body.of_stream)
  (Uri.of_string ("http://0.0.0.0:8000" ^ query)) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  let received_body = body |> Cohttp_lwt_body.to_string in
  return (code, received_body)

let get_body query =
  Client.get
  (Uri.of_string ("http://0.0.0.0:8000" ^ query)) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  let received_body = body |> Cohttp_lwt_body.to_string in
  return (code, received_body)

(* [send_json j} sends a json to the servers. Returns diff list *)
let send_post_request (j: diff_json) (action: string) (client_id: int) =
  let query = make_query action client_id in
  Lwt_main.run (post_body j query) |> return

(* [send_json j} sends a json to the servers. Returns diff list *)
let send_get_request (action: string) (client_id: int) =
  let query = make_query action client_id in
  Lwt_main.run (get_body query) |> return

let send_login_request (name: string) =
  let query = make_login_query name in
  Lwt_main.run (get_body query) |> return