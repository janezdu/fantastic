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

type mode = Login of string | Query of int | Badmode

let loginsallowed = ref true


let handleQuery req body cid =
  match Uri.path (Request.uri req) with
  | "/move" -> begin
      body |> Cohttp_lwt_body.to_string >|= (fun cmdbody ->
          ( print_endline cmdbody;
            pushClientUpdate cid cmdbody "move"))
      >>= (fun body -> Server.respond_string ~status:`OK ~body ())
    end
  | "/update" -> begin
      body |> Cohttp_lwt_body.to_string >|= (fun cmdbody ->
          ( print_endline ("[UPDATE]: "^(string_of_int cid));
            getClientUpdate cid))
      >>= (fun body -> Server.respond_string ~status:`OK ~body ())
    end
  | _ -> begin
      Server.respond_string ~status:`Bad_request ~body: "u dun guffed off" ()
    end

let handleLogin req body name =
  let cid = registerUser name in
  print_endline (name ^ "'s clieentid is "^(string_of_int cid));
  Server.respond_string ~status:`OK ~body: (string_of_int cid) ()


(* a server is a function that gets data, compute and respond *)
let server =
  let callback _conn req body =
    try
      print_endline ("\n\n===================================================="^
                     "\nstarted callback");
      print_endline (req |> Request.uri |> Uri.to_string);
      print_endline (Uri.path (Request.uri req));
      (* let uri = req |> Request.uri |> Uri.to_string in *)
      (* let meth = req |> Request.meth |> Code.string_of_method in *)
      (* let headers = req |> Request.headers |> Header.to_string in *)
      let queryparams = req |> Request.uri |> Uri.query in

      let reqmode =
        if List.mem_assoc "client_id" queryparams then
          let cid = List.assoc "client_id" queryparams
                    |> List.hd |> int_of_string in
          (* print_endline ("New player: " ^ string_of_int cid); *)
          Query (List.assoc "client_id" queryparams |> List.hd |> int_of_string)
        else if List.mem_assoc "username" queryparams then
          begin
            let name = List.assoc "username" queryparams |> List.hd in
            (* print_endline ("New player: " ^ name); *)
            Login (name)
          end
        else Badmode
      in

      match reqmode with
      | Query (cid) -> begin
          print_endline ("hanlding player "^(string_of_int cid));
          loginsallowed := false;
          handleQuery req body cid
        end
      | Login (name) -> begin
          print_endline ("hanlding login "^name);
          if !loginsallowed then handleLogin req body name
          (* TODO rolling login: keep list of old diffs in state *)
          else raise (BadRequest ("too late to log in"))
      end
      | Badmode -> raise (BadRequest ("Badly formed uri, missing query"))

    with
    | WorldFailure msg -> (Server.respond_string ~status:`OK ~body:"no username" ())
    | BadRequest msg -> Server.respond_string ~status:`Bad_request ~body:"" ()
    (* | _ -> Server.respond_string ~status:`Bad_request ~body:"idk even" () *)

  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
