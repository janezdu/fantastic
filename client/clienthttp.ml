open Lwt
open Cohttp
open Cohttp_lwt_unix


type diff_json = string


(* stuff constant *)
let ok = 200
let forbidden = 403

let make_query_helper action cid =
  "/" ^ action ^ "?client_id=" ^ (string_of_int cid)

let make_query action cid =
  match action with
  | "move" -> make_query_helper "move" cid
  | "use" -> make_query_helper "use" cid
  | "take" -> make_query_helper "take" cid
  | "drop" -> make_query_helper "drop" cid
  | "quit" -> make_query_helper "quit" cid
  | _ -> failwith "invalid command"

let post_body jstr query (callback:string -> 'a) =
  Client.post ~body:([jstr] |> Lwt_stream.of_list |> Cohttp_lwt_body.of_stream)
  (Uri.of_string ("http://0.0.0.0:8000" ^ query)) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  if code = ok then
    body |> Cohttp_lwt_body.to_string >|= callback
  else failwith "403 Forbidden: illegal move"

let get_body query (callback:string -> 'a) =
  Client.get
  (Uri.of_string ("http://0.0.0.0:8000" ^ query)) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  if code = ok then
    body |> Cohttp_lwt_body.to_string >|= callback
  else failwith "403 Forbidden: illegal move"


(* [send_json j} sends a json to the servers. Returns unit *)
let send_post_request (j: diff_json) (action: string)
  (client_id: int) (callback: string -> 'a) =
  let query = make_query action client_id in
  Lwt_main.run (post_body j query callback)

(* [send_json j} sends a json to the servers. Returns unit *)
let send_get_request (action: string)
  (client_id: int) (callback: string -> 'a) =
  let query = make_query action client_id in
  Lwt_main.run (post_body j query callback)