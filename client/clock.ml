(*
 * clock.ml
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Controller
open Lwt_react
open Lwt
open LTerm_widget

let get_time () =
  let localtime = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%02u:%02u:%02u"
    localtime.Unix.tm_hour
    localtime.Unix.tm_min
    localtime.Unix.tm_sec

let get_clientid () = print_endline ("getting id " ^ string_of_int (!client_id)); (string_of_int !client_id)

let main () =
  let waiter, wakener = wait () in

  let vbox = new vbox in
  let health = new label (get_clientid ()) in
  let button = new button "exit" in
  vbox#add health;
  vbox#add button;

  (* Update the time every second. *)
  ignore (Lwt_engine.on_timer 1.0 true (fun _ -> health#set_text (get_clientid ())));
  (* get_check !client_id !curr_w *)

  (* Quit when the exit button is clicked. *)
  button#on_click (wakeup wakener);

  (* Run in the standard terminal. *)
  Lazy.force LTerm.stdout
  >>= fun term ->
  run term vbox waiter

let () = Lwt_main.run (main ())
