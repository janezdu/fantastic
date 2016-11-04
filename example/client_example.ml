(* client_example.ml
 * follow this link on terminal: https://github.com/mirage/ocaml-cohttp *)
open Lwt
open Cohttp
open Cohttp_lwt_unix

(* read HTTP protocol: POST - send something to server, GET - get data from
 * server
 * decode the body
 * find type of header: XML or JSON = method data cache.
 *  - can inspect element -> ex: content-type: text/html
 * to build: ocamlbuild -pkg cohttp.lwt client_example.native
 * to run: ./client_example.native *)

(* >>= is "bind" for call back function (async program)
 * note: async != multithread
 * multithread = two separate programs run parallel, independently
 * async = one program, but runs everything at the same time.
 * for functions that takes time -> let other thread to compute and will
 * come back to get the results later *)
let body =
  Client.get (Uri.of_string "http://0.0.0.0:8000") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  (* below is the function of the client call *)
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt_body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let () =
  let body = Lwt_main.run body in
  print_endline ("Received body\n" ^ body)