open Lwt
open Cohttp
open Cohttp_lwt_unix

type jsonstring = string

(* [send_post_request j} sends a command json string to the servers.
 * Returns unit *)
val send_post_request: jsonstring -> string -> int -> (string -> 'a) -> 'a

(* [send_post_request j} sends a get request to the servers. Returns unit *)
val send_get_request: jsonstring -> string  -> int -> (string -> 'a) -> 'a
