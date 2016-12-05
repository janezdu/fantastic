(*
 * repl.ml
 * --------
 * Copyright : (c) 2015, Martin DeMello <mdemello@google.com>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open React
open Lwt
open LTerm_text
open CamomileLibraryDyn.Camomile
open LTerm_style
open LTerm_geom
open Controller
open Model

let dead_warning_msg = "The dead can't use that command. Too bad!"
let invalid_command_msg = "Invalid command. Please try again.\n"
let trouble_connection_msg = "There is a problem with the connection. "^
  "Please check the connection and enter the ip address of the host again\n"

let hp = ref 1000

(* +-----------------------------------------------------------------+
   | Interpreter                                                     |
   +-----------------------------------------------------------------+ *)

(* A simple model of an interpreter. It maintains some state, and exposes a function
 *   eval : state -> input -> (new_state, output) *)
module Interpreter = struct
  type state = { n : int;
                 world : world;
                 clientid : int ref}

  let eval state s =
    repl_helper s state.world >>= fun newworld ->
    return { state with n = state.n + 1; world = newworld}
end

(* +-----------------------------------------------------------------+
   | Prompt and output wrapping                                      |
   +-----------------------------------------------------------------+ *)

let get_hp_prompt state =
  get_hp !(state.Interpreter.clientid) (state.Interpreter.world.items)

let get_score_prompt state =
  get_score !(state.Interpreter.clientid) (state.Interpreter.world.items)

let get_curr_loc_prompt state =
  get_curr_loc (state.Interpreter.world.players) |> string_of_int_tuple

(* Create a prompt based on the current interpreter state *)
let make_prompt size state =
  ignore (Lwt_engine.on_timer 0.2 true (fun _ ->
    hp := get_hp_prompt state));
  let prompt = Printf.sprintf "Next? [%d]: " state.Interpreter.n in
  let score_string = Printf.sprintf "score: %d" (get_score_prompt state) in
  let room_string = Printf.sprintf "loc: %s" (get_curr_loc_prompt state) in
  eval [
  B_bold true;
  B_fg lgreen;
  S"─( ";
  B_fg lred; S(Printf.sprintf "hp: %d" (!hp)); E_fg;
  S" )─< ";
  B_fg lcyan; S(score_string); E_fg;
  S" >─";
  S(Zed_utf8.make
      (size.cols - 24 - Zed_utf8.length room_string
      - Zed_utf8.length score_string)
      (UChar.of_int 0x2500));
  S"[ ";
  B_fg(lmagenta); S room_string; E_fg;
  S" ]─";
  E_fg;
  S"\n";
    S prompt ]

(* Format the interpreter output for REPL display *)
let make_output () =
  eval [ S "" ]

(* +-----------------------------------------------------------------+
   | Customization of the read-line engine                           |
   +-----------------------------------------------------------------+ *)

let time =
  let time, set_time = S.create (Unix.time ()) in
  (* Update the time every second. *)
  ignore (Lwt_engine.on_timer 1.0 true (fun _ -> set_time (Unix.time ())));
  time

class read_line ~term ~history ~state = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method show_box = false

  initializer
    self#set_prompt (S.l2 (fun size time -> make_prompt size state) self#size time)
end

(* +-----------------------------------------------------------------+
   | Main loop                                                       |
   +-----------------------------------------------------------------+ *)

let rec loop term history state =
  Lwt.catch (fun () ->
    let rl = new read_line ~term ~history:(LTerm_history.contents history)
      ~state in
    rl#run >|= fun command -> Some command)
  (function
    | Sys.Break -> return None
    | exn -> Lwt.fail exn)
  >>= function
  | Some command ->
    Lwt.catch (fun () ->
      Interpreter.eval state command >>= fun new_state ->
      LTerm.fprintls term (make_output ())
      >>= fun () ->
      LTerm_history.add history command;
      loop term history new_state)
    (incorrect_command_handler term history state)
  | None ->
    loop term history state

and incorrect_command_handler term history state = function
  | Dead -> (print_endline dead_warning_msg; loop term history state)
  | _ -> (print_endline invalid_command_msg; loop term history state)

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let rec main () =
  LTerm_inputrc.load ()
  >>= fun () ->
  Lwt.catch (fun () ->
    Lwt.catch (fun () -> loadin ()) (loadin_exn_handler)
    >>= fun world ->
    let state =
      {Interpreter.n = 1; Interpreter.world = world; clientid = client_id} in
    Lazy.force LTerm.stdout
    >>= fun term ->
    loop term (LTerm_history.create []) state)
  (main_exn_handler)

and main_exn_handler = function
  | LTerm_read_line.Interrupt -> Lwt.return ()
  | Sys_error explanation ->
    (print_endline explanation;
    print_string "\n> ";
     main ())
  | Unix.Unix_error _ ->
    (print_endline (trouble_connection_msg);
    print_string "> ";
     main ())
  | _ -> (print_endline (trouble_connection_msg);
    print_string "> ";
     main ())

and loadin_exn_handler = function
  | Sys_error explanation ->
    (print_endline explanation;
    print_string "\n> ";
     loadin ())
  | Unix.Unix_error _ ->
    (print_endline (trouble_connection_msg);
    print_string "> ";
     loadin ())
  | _ -> (print_endline (trouble_connection_msg);
    print_string "> ";
     loadin ())

let () = Lwt_main.run (main ())