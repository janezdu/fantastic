open Lwt
open Cohttp
open Cohttp_lwt_unix

open Yojson.Basic.Util
open Controller

type status = OK | Invalid
type json = Yojson.Basic.json

exception WorldFailure of string
exception BadRequest of string

(* [start pw] will start a game server; join game with password [pw] *)
let start s = failwith "unimplemented"

(* [translate_to_newuser j] will attempt to add the new user with the password
 * they have included in [j], returning a sessionid if valid. *)
let translate_to_newuser j = failwith "unimplemented"

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
    try



      print_endline "started callback";
      print_endline (req |> Request.uri |> Uri.to_string);
      print_endline (Uri.path (Request.uri req));
      (* let uri = req |> Request.uri |> Uri.to_string in *)
      (* let meth = req |> Request.meth |> Code.string_of_method in *)
      (* let headers = req |> Request.headers |> Header.to_string in *)
      let queryparams = req |> Request.uri |> Uri.query in

      let cid = begin
        try List.assoc "username" queryparams |> List.hd |> int_of_string
        with
        | _ -> raise (BadRequest ("No username selected"));
      end in
      (* let mvregexp = Str.regexp ".*move.*" in

         if Str.string_match mvregexp uri 0 then *)
      match Uri.path (Request.uri req) with
      | "/move" -> begin
          print_endline "got to move";
          body |> Cohttp_lwt_body.to_string >|= (fun cmdbody ->
            ( print_endline cmdbody;
              pushClientUpdate cid cmdbody "move"))
          >>= (fun body -> Server.respond_string ~status:`OK ~body ())
        end
      | "/update" -> begin
          print_endline "got to update";
          body |> Cohttp_lwt_body.to_string >|= (fun cmdbody ->
              ("not done implementing update tho" ))
          >>= (fun body -> Server.respond_string ~status:`OK ~body ())
        end
      | _ -> Server.respond_string ~status:`Bad_request ~body: "u dun guffed off" ()
      (* else failwith "Can only handle move" *)
      (*   begin
          body |> Cohttp_lwt_body.to_string >|= (fun body ->
          (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
             uri meth headers body))
          >>= (fun body -> Server.respond_string ~status:`OK ~body ())
        end *)
(*         (* GET Request *)
        (* let resbody = translate_to_json (getClientUpdate cmd json cid) in *)


      (* after code that parses POST update *)
      body (*...*)


      if x then
          body |> Cohttp_lwt_body.to_string >|= (fun body ->
            (Printf.sprintf "We're fantastic!"))
          >>= (fun body -> Server.respond_string ~status:`OK ~body ()) *)
    with
    | WorldFailure msg -> (Server.respond_string ~status:`OK ~body:"no username" ())
    | BadRequest msg -> Server.respond_string ~status:`Bad_request ~body:"" ()

  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
