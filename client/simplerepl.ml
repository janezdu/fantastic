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

(* Create a prompt based on the current interpreter state *)
let make_prompt size state =
  let prompt = Printf.sprintf "Next? [%d]: " state.Interpreter.n in
  let scorestring = Printf.sprintf "score: %d" (get_score_prompt state) in
  eval [
  B_bold true;
  B_fg lcyan;
  S"─( ";
  B_fg lmagenta; S(Printf.sprintf "hp: %d" (get_hp_prompt state)); E_fg;
  S" )─< ";
  B_fg green; S(scorestring); E_fg;
  S" >─";
  S(Zed_utf8.make
      (size.cols - 24 - Zed_utf8.length "code" - Zed_utf8.length scorestring)
      (UChar.of_int 0x2500));
  S"[ ";
  B_fg(lred); S "code"; E_fg;
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

let main () =
  LTerm_inputrc.load ()
  >>= fun () ->
  Lwt.catch (fun () ->
    loadin () >>= fun world ->
    let state =
      {Interpreter.n = 1; Interpreter.world = world; clientid = client_id} in
    Lazy.force LTerm.stdout
    >>= fun term ->
    loop term (LTerm_history.create []) state)
  (function
    | LTerm_read_line.Interrupt -> Lwt.return ()
    | exn -> Lwt.fail exn)

let () = Lwt_main.run (main ())