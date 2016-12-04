
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

let post_body ip jstr query =
  Client.post ~body:([jstr] |> Lwt_stream.of_list |> Cohttp_lwt_body.of_stream)
  (Uri.of_string ("http://" ^ ip ^":8000" ^ query)) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  let received_body = body |> Cohttp_lwt_body.to_string in
  return (code, received_body)

let get_body ip query =
  Client.get
  (Uri.of_string ("http://" ^ ip ^":8000" ^ query)) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  let received_body = body |> Cohttp_lwt_body.to_string in
  return (code, received_body)

(* [send_json j} sends a json to the servers. Returns diff list *)
let send_post_request (ip: string) (j: diff_json)
  (action: string) (client_id: int) =
  let query = make_query action client_id in
  post_body ip j query

(* [send_json j} sends a json to the servers. Returns diff list *)
let send_get_request (ip: string)  (action: string) (client_id: int) =
  let query = make_query action client_id in
  get_body ip query

let send_login_request (ip: string) (name: string) =
  let query = make_login_query name in
  get_body ip query
