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
let send_response j s = failwith "unimplemented"

(* [send_status] sense a response without a body*)
let send_status s = failwith "unimplemented"


let x = true

(* a server is a function that gets data, compute and respond *)
let server =
(*     let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt_body.to_string >|= (fun body ->
      (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
         uri meth headers body))
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ()) *)
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    let cid = 1234 in (* ... something with queries  *)

    let mvregexp = Str.regexp ".*move.*" in

    if Str.string_match mvregexp uri 18 then
      begin
(*         let cmd = body |> Cohttp_lwt_body.to_string in
        print_endline cmd; *)
        (* if pushClientUpdate cid cmd then (* send OK *) *)
        
        body |> Cohttp_lwt_body.to_string >|= 
          (
            fun cmdbody -> pushClientUpdate cid cmdbody
            (Printf.sprintf translate_to_diff )
          )


        >>= (fun body -> Server.respond_string ~status:`OK ~body ())

        else (* send BadMove *) 
      end
    
    else
      begin
        body |> Cohttp_lwt_body.to_string >|= (fun body ->
        (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
           uri meth headers body))
        >>= (fun body -> Server.respond_string ~status:`OK ~body ())
      end

      (* GET Request *)
      (* let resbody = translate_to_json (getClientUpdate cmd json cid) in *)
    

    (* after code that parses POST update *)
    body (*...*)
    

    if x then
        body |> Cohttp_lwt_body.to_string >|= (fun body ->
          (Printf.sprintf "We're fantastic!"))
        >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)