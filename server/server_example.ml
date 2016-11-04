open Lwt
open Cohttp
open Cohttp_lwt_unix

(* server communicates state of the game
 * follow this link on terminal: https://github.com/mirage/ocaml-cohttp
 * to build: ocamlbuild -pkg cohttp.lwt server_example.native
 * to run: ./server_example.native *)

(* a server is a function that gets data, compute and respond *)
let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt_body.to_string >|= (fun body ->
      (Printf.sprintf "We're fantastic!"))
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)