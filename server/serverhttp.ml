open Lwt
open Cohttp
open Cohttp_lwt_unix

open Yojson.Basic.Util
open Controller

type status = OK | Invalid
type json = Yojson.Basic.json

exception WorldFailure = Controller.WorldFailure
exception EndGame = Controller.EndGame
exception BadRequest of string

let debugging = Controller.debugging
let p msg = if debugging then print_endline msg else ignore ()


(* [start pw] will start a game server*)
let start () = failwith "unimplemented"

type mode = Login of string | Query of int | Badmode

let strip path = String.sub path 1 (String.length path - 1)

(* let loginsallowed = ref true *)

(* A method that handles legal queries once gameplay starts. Does not handle
 * login *)
let handleQuery req body cid : string Lwt.t =
  try
    let path = Uri.path (Request.uri req) in
    match path with
    | "/move" | "/use" | "/take" | "/drop"  -> begin
        body |> Cohttp_lwt_body.to_string >>= (fun cmdbody ->
            ( (*print_endline ("\n\n===================================================="^
                             "\nstarted callback for POST request");
              print_endline ("Body: "^ cmdbody); *)
              try
                return (pushClientUpdate cid cmdbody (strip path))
              with
              | WorldFailure msg -> begin
                  print_endline msg;
                  Lwt.fail (WorldFailure msg)
                end))
      end
    | "/quit" -> begin
        try
          return (pushClientUpdate cid "{}" (strip path))
        with
        | WorldFailure msg -> begin
            print_endline msg;
            Lwt.fail (WorldFailure msg)
          end
      end
    | "/update" -> body |> Cohttp_lwt_body.to_string >|= (fun cmdbody ->
        (
          (* print_endline ("[UPDATE]: "^(string_of_int cid)); *)
          getClientUpdate cid))
    | _ -> begin return ("Bad URI; not in API")
      (* Server.respond_string ~status:`Bad_request ~body: "u dun guffed off" () *)
      end
  with
  | x -> raise x

(* A method that deals with user registration only. *)
let handleLogin req body name =
  let cid = registerUser name in
  print_endline (name ^ "'s clieentid is "^(string_of_int cid));
  return (string_of_int cid)

(* a server is a function that gets data, compute and respond *)
let server =
  let callback _conn req body =
    p ("\n\n===================================================="^
                   "\nstarted callback");
    p (req |> Request.uri |> Uri.to_string);
    let queryparams = req |> Request.uri |> Uri.query in

    let reqmode =
      if List.mem_assoc "client_id" queryparams then
        let cid = List.assoc "client_id" queryparams
                  |> List.hd |> int_of_string in
        Query (cid)
      else if List.mem_assoc "username" queryparams then
        begin
          let name = List.assoc "username" queryparams |> List.hd in
          Login (name)
        end
      else Badmode
    in

    let errorhandler = function
      | EndGame winner -> begin
          print_endline "EndGame errorhandler";
          let msg = ("The game has ended! Congratulations, "^winner^"!") in
          print_endline msg;
          Server.respond_string ~status:`Created ~body:msg ()
      end
      | WorldFailure msg -> begin
          print_endline msg;
          Server.respond_string ~status:`Bad_request ~body:msg ()
        end
      | BadRequest msg -> Server.respond_string ~status:`Bad_request ~body:msg ()
    in

    match reqmode with
      | Query (cid) -> begin
          Lwt.catch (fun () ->
              handleQuery req body cid >>=
              (fun body -> Server.respond_string ~status:`OK ~body ())
            ) errorhandler
        end
      | Login (name) -> begin
          print_endline ("handling login "^name);
          Lwt.catch (fun () ->
              handleLogin req body name >>=
              (fun body -> Server.respond_string ~status:`OK ~body ())
            ) errorhandler
        end
      | Badmode -> raise (BadRequest ("Badly formed uri, missing query"))

  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
