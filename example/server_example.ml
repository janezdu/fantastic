open Lwt
open Cohttp
open Cohttp_lwt_unix

(* server communicates state of the game
 * follow this link on terminal: https://github.com/mirage/ocaml-cohttp
 * to build: ocamlbuild -pkg cohttp.lwt server_example.native
 * to run: ./server_example.native *)

(* a server is a function that gets data, compute and respond *)

module LibMap = Map.Make (
    struct
      type t = int
      let compare e1 e2 = compare e1 e2
    end )

let id_map = ref (LibMap.empty: int LibMap.t)
(* let login = 
  let next_id = ref 1000 in 
  let callback _conn req body =
    let uri = req |> Request.uri in
    let username = match Uri.get_query_param uri "username" with
      | Some name -> name
      | None -> failwith "No username"
    in 
    let id = !next_id in
    next_id := (!next_id + 1);
    id_map := LibMap.add id username (!id_map);
    body |> Cohttp_lwt_body.to_string >|= (fun body ->
      (Printf.sprintf "Your id number is %s" string_of_int id))
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())
 *)

let login =
  let next_id = ref 1000 in 
  let callback _conn req body =
    let raw_uri = req |> Request.uri in
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    let username = match Uri.get_query_param raw_uri "username" with
      | Some name -> name
      | None -> failwith "No username"
    in 
    let id = !next_id in
    next_id := (!next_id + 1);
    body |> Cohttp_lwt_body.to_string >|= (fun body ->
      (Printf.sprintf "id: %s\nUri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
         (string_of_int id) uri meth headers body))
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())


(* let server =
  let x = ref 0 in
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    let n = !x in
    x := (!x + 1);
    body |> Cohttp_lwt_body.to_string >|= (fun body ->
      (Printf.sprintf "X: %s\nUri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
         (string_of_int n) uri meth headers body))
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())
 *)
let () = ignore (Lwt_main.run login)