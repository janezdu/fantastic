(*
 * repl.ml
 * --------
 * Copyright : (c) 2015, Martin DeMello <mdemello@google.com>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(* Add a REPL to an existing interpreter *)

open React
open Lwt
open LTerm_text
open CamomileLibraryDyn.Camomile
open LTerm_style
open LTerm_geom

open Controller

(* +-----------------------------------------------------------------+
   | Interpreter                                                     |
   +-----------------------------------------------------------------+ *)

(* A simple model of an interpreter. It maintains some state, and exposes a function
 *   eval : state -> input -> (new_state, output) *)
module Interpreter = struct
  type state = { n : int ;
                 hp : int ;
                 world : world}


  let eval state s =
    print_endline " called eval";
    let out = "evaluated " ^ s in
    repl_helper s state.world >>= fun (newworld) ->
    let new_state = { state with n = state.n + 1; world = newworld } in
    return ((new_state, out))
end

(* +-----------------------------------------------------------------+
   | Prompt and output wrapping                                      |
   +-----------------------------------------------------------------+ *)

(* Create a prompt based on the current interpreter state *)
let make_prompt size state =
  (* print_endline "make_prompt"; *)
  let prompt = Printf.sprintf "In  [%d]: " state.Interpreter.n in
  eval [
  B_bold true;

  B_fg lcyan;
  S"─( ";
  B_fg lmagenta; S(Printf.sprintf "HP: %d" state.Interpreter.hp); E_fg;
  S" )─< ";
  B_fg lyellow; S "hllo"; E_fg;
  S" >─";
  S(Zed_utf8.make
      (size.cols - 24 - Zed_utf8.length "code" - 4)
      (UChar.of_int 0x2500));
  S"[ ";
  B_fg(lred); S "code"; E_fg;
  S" ]─";
  E_fg;
  S"\n";
    S prompt ]

(* Format the interpreter output for REPL display *)
let make_output state out =
  let output = Printf.sprintf "Out [%d]: %s" state.Interpreter.n out in
  eval [ S output ]

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
      let rl = new read_line ~term ~history:(LTerm_history.contents history) ~state in
      rl#run >|= fun command -> Some command)
    (function
      | Sys.Break -> return None
      | exn -> Lwt.fail exn)
  >>= function
  | Some command ->
    (* let state, out = Interpreter.eval state command in
       LTerm.fprintls term (make_output state out) *)
    Interpreter.eval state command >>= fun (state, out) ->
    (* LTerm.fprintls term (make_output state out) *)
    return ()

    >>= fun () ->
    LTerm_history.add history command;
    loop term history state
  | None ->
    loop term history state

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let main () =
  LTerm_inputrc.load ()
  >>= fun () ->
  Lwt.catch (fun () ->
      loadin () >>= fun world ->
      let state = { Interpreter.n = 1;Interpreter.hp = 300; Interpreter.world = world } in
      Lazy.force LTerm.stdout
      >>= fun term ->
      loop term (LTerm_history.create []) state)
    (function
      | LTerm_read_line.Interrupt -> Lwt.return ()
      | exn -> Lwt.fail exn)

let () = Lwt_main.run (main ())
