open Str

(* command is a variant type of the possible commands a player
 * could make *)
type command =
  | Move of string
  | Drink of string
  | Spell of string*string
  | Quit
  | Take of string
  | Drop of string
  | Look
  | Inventory
  | ViewState
  | Help
  | Check
  | Checkout of string

let sep_target t =
  let comm_str = (String.trim t) in
  let comm_lst = Str.split (Str.regexp ",") comm_str in
  let comm_name = String.trim (List.nth comm_lst 0) in
  let comm_target = String.trim (List.nth comm_lst 1) in
  let comm_tup =  (comm_name, comm_target) in
  comm_tup

(* [parse_command lst] is the command that is associated with [lst]
 * Raises Illegal if [lst] does represent to in a valid command form
 * requires: [lst] is a list of at most length 2
 *           if [lst] represents a valid command it should be in form
 *           ["command variant"; "object of command"]
 *           ["direction of go command"]
 *)
let parse_command (lst)=
    match lst with
    | h::t::[] when (String.trim h="move") -> Move (String.trim t)
    | h::t::[] when (String.trim h="drink") ->
      (*let tup = sep_target t in
      Drink (fst tup, snd tup)*)
      Drink (String.trim t)
    | h::t::[] when (String.trim h="spell") ->
      (*let spell_str = (String.trim t) in
      let spell_lst = Str.split (Str.regexp ",") spell_str in
      let spell_name = String.trim (List.nth spell_lst 0) in
      let spell_target = String.trim (List.nth spell_lst 1) in
      let spell_tup =  (spell_name, spell_target ) in*)
      let tup = sep_target t in
      (*Spell (spell_name,spell_target)*)
      Spell (fst tup, snd tup)
    | h::t::[] when (String.trim h="drop") -> Drop (String.trim t)
    | h::t::[] when (String.trim h="take") -> Take (String.trim t)
    | h::t::[] when (String.trim h="checkout") -> Checkout (String.trim t) 
    | h::[] when (String.trim h="look") -> Look
    | h::[] when (String.trim h="quit")-> Quit
    | h::[] when (String.trim h="inv" || String.trim h = "inventory") ->
        Inventory
    | h::[] when (String.trim h="view")-> ViewState
    | h::[] when (String.trim h="help") -> Help
    | h::[] when (String.trim h="check") -> Check
    | h::[] -> Move (String.trim h)(*new stuff*)
    | _ -> failwith "Illegal"

(* [split_to_list str] is a string that results from splitting [str] into a
 * list using the whitespace in [str] as places for separation
 * EX: (split_to_st "  hello  world    ") would split to ["hello"; "world"]
 * requires: [str] is a string
 *)
let rec split_to_list str =
    let l = String.length str in
    match l with
    | 0 -> []
    | _ when String.contains str ' '->
        begin
        let first_space = String.index str ' ' in
        let word = String.sub str 0 first_space in
        let length = l-first_space in
        let substring = String.sub str first_space length in
        let new_str = String.trim substring in
        [word] @ (split_to_list new_str)
        end
    | _ -> [str]

(* [list_concat lst] is the string that results from concatenating
 * the elements in lst together with one spade " " separating each element in
 * lst
 * requires: [lst] is a string list*)
let rec list_concat lst =
    match lst with
    | [] -> ""
    | h::t -> h^" "^(list_concat t)

(* [sep_dir lst] is the string list with length of at most 2 that results
 * from making list [lst] into the form ["command variant"; "object of command"]
 * or ["direction of go command"]
 * Raises Illegal if [lst] is empty
 * requires: [lst] is a string list
 *)
let sep_dir lst =
    match lst with
    | h::[] -> h::[]
    | h::t when (h = "move" || h = "take" || h = "drink" || h = "spell" || h = "drop" || h = "checkout")->
      h::(String.trim (list_concat t))::[]
    | h::t -> (String.trim (list_concat lst))::[]
    | _ -> failwith "Illegal"

(* [parse_c command] parses the command [command] into a command variant
 * raises Illegal if [command] not in the form of a valid command
 * requires: [command] is  a string*)
let parse_c command =
    let c = String.trim (String.lowercase_ascii command) in
    let spl = split_to_list c in
    let sep_com = sep_dir spl in
    parse_command sep_com

(* [parse_comm d] is the command type that results from the player's
 * typed directrive. *)
let parse_comm d = parse_c d