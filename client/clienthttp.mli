
open Lwt
open Cohttp
open Cohttp_lwt_unix

type diff_json = string

(* [send_post_request j} sends a command json string to the servers.
 * Returns unit *)
val send_post_request: string -> diff_json -> string -> int -> (int * string Lwt.t) Lwt.t

(* [send_post_request j} sends a get request to the servers. Returns unit *)
val send_get_request: string -> string -> int -> (int * string Lwt.t) Lwt.t

(* [send_login_request name] takes in [name] of the user and
 * requests client_id from the server and returns
 * string of int in the body of the response with code 200 *)
val send_login_request: string -> string -> (int * string Lwt.t) Lwt.t