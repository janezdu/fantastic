open Lwt
open Cohttp
open Cohttp_lwt_unix

open Yojson.Basic.Util
open Controller

type status = OK | Invalid
type json = Yojson.Basic.json


(* [start pw] will start a game server; join game with password [pw] *)
let start s = failwith "unimplemented"

(* [translate_to_newuser j] will attempt to add the new user with the password
 * they have included in [j], returning a sessionid if valid. *)
let translate_to_newuser j = failwith "unimplemented"

(* [translate_to_diff j] returns diffs based on a json *)
let translate_to_diff j = failwith "unimplemented"

(* [translate_to_json d] returns a json based on diffs *)
let translate_to_json diffs  = failwith "unimplemented"

(* [send_response j status] sends an http response to the clients *)
let send_respoonse j s = failwith "unimplemented"

(* [send_status] sense a response without a body*)
let send_status s = failwith "unimplemented"



let x = true

(* a server is a function that gets data, compute and respond *)
let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in

    if x then 
        body |> Cohttp_lwt_body.to_string >|= (fun body ->
          (Printf.sprintf "We're fantastic!"))
        >>= (fun body -> Server.respond_string ~status:`OK ~body ())
    else
         body |> Cohttp_lwt_body.to_string >|= (fun body ->
          (Printf.sprintf "Fantastic logical stuff!"))
        >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)