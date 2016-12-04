open OUnit2
open Model
open Clienthttp
open Controller
open Cli

let j1 = Yojson.Basic.from_file "fourrooms.json" 
let w1 = Controller.init_state j1

let parse_comm_tests =
 [ 
  

  "parse_comm move north" >:: 
      (fun _ -> assert_equal (Move "north") (parse_comm "north"));
  "parse_comm move north" >:: 
      (fun _ -> assert_equal (Move "north") (parse_comm "    north      "));
  "parse_comm move north" >:: 
      (fun _ -> assert_equal (Move "north") (parse_comm "NoRth"));
  "parse_comm move north" >:: 
      (fun _ -> assert_equal (Move "north") (parse_comm "    NORTH"));
  "parse_comm move north" >:: 
      (fun _ -> assert_equal (Move "north") (parse_comm "noRth   "));
  "parse_comm move north" >:: 
      (fun _ -> assert_equal (Move "north") (parse_comm "   move   north   "));

  "parse_coom take item1" >:: 
      (fun _ -> assert_equal (Take "item1") (parse_comm "take item1    " ));
  "parse_coom take item1" >:: 
      (fun _ -> assert_equal (Take "item1") (parse_comm "     take item1" ));
  "parse_coom take item1" >:: 
      (fun _ -> assert_equal (Take "item1") (parse_comm "    take    item1   "));
  "parse_coom take item1" >:: 
      (fun _ -> assert_equal (Take "item1") (parse_comm "    tAKe    iTEm1   "));

  "parse_coom drop item1" >:: 
      (fun _ -> assert_equal (Drop "item1") (parse_comm "drop item1    " ));
  "parse_coom drop item1" >:: 
      (fun _ -> assert_equal (Drop "item1") (parse_comm "     Drop item1" ));
  "parse_coom drop item1" >:: 
      (fun _ -> assert_equal (Drop "item1") (parse_comm "   DROP   item1   "));
  "parse_coom drop item1" >:: 
      (fun _ -> assert_equal (Drop "item1") (parse_comm "    dRop   iTEm1   "));

  "parse_coom drink item1" >:: 
      (fun _ -> assert_equal (Drink "item1") 
          (parse_comm "drink item1    " ));
  "parse_coom drink item1" >:: 
      (fun _ -> assert_equal (Drink "item1") (parse_comm "     Drink item1" ));
  "parse_coom drink item1" >:: 
      (fun _ -> assert_equal (Drink "item1") 
          (parse_comm "   DRINK   item1   "));
  "parse_coom drink item1" >:: 
      (fun _ -> assert_equal (Drink "item1") 
          (parse_comm "    dRink   iTEm1   "));

  "parse_coom spell expelliarmus bob" >:: 
      (fun _ -> assert_equal (Spell ("expelliarmus", "bob")) 
          (parse_comm "spell expelliarmus, bob" ));
  "parse_coom spell expelliarmus bob" >:: 
      (fun _ -> assert_equal (Spell ("expelliarmus", "bob")) 
          (parse_comm "     spell expelliarmus, bob" ));
  "parse_coom spell expelliarmus bob" >:: 
      (fun _ -> assert_equal (Spell ("expelliarmus", "bob")) 
          (parse_comm "spell expelliarmus, bob       "));
  "parse_coom spell expelliarmus bob" >:: 
      (fun _ -> assert_equal (Spell ("expelliarmus", "bob")) 
          (parse_comm "   sPeLl     expelliarmus     , bOb "));

  "parse_coom spell avada kedavra bob" >:: 
      (fun _ -> assert_equal (Spell ("avada kedavra", "bob")) 
          (parse_comm "spell avada kedavra, bob" ));
  "parse_coom spell avada kedavra bob" >:: 
      (fun _ -> assert_equal (Spell ("avada kedavra", "bob")) 
          (parse_comm "     spell avada   kedavra, bob" ));
  "parse_coom spell avada kedavra bob" >:: 
      (fun _ -> assert_equal (Spell ("avada kedavra", "bob")) 
          (parse_comm "spell    avada kedavra, bob       "));
  "parse_coom spell avada kedavra bob" >:: 
      (fun _ -> assert_equal (Spell ("avada kedavra", "bob")) 
          (parse_comm "   sPeLl     avada     kedavra    , bOb "));


  "parse_coom look" >:: 
      (fun _ -> assert_equal Look (parse_comm "look" ));
  "parse_coom look" >:: 
      (fun _ -> assert_equal Look (parse_comm " LoOk    " ));
  "parse_coom look" >:: 
      (fun _ -> assert_equal Look (parse_comm "LOOK" ));
  

  "parse_comm quit" >:: 
      (fun _ -> assert_equal Quit (parse_comm "quit"));
  "parse_comm quit" >:: 
      (fun _ -> assert_equal Quit (parse_comm "QUIT" ));
  "parse_comm quit" >:: 
      (fun _ -> assert_equal Quit (parse_comm " QuIt    "));
 

  "parse_comm view" >:: 
      (fun _ -> assert_equal ViewState (parse_comm  "view" ));
  "parse_comm view" >:: 
      (fun _ -> assert_equal ViewState (parse_comm  "     VIeW    " ));
  "parse_comm view" >:: 
      (fun _ -> assert_equal ViewState (parse_comm  "VIEW" ));
  

  "parse_comm 'inv' " >:: 
      (fun _ -> assert_equal Inventory (parse_comm "INv" ));
  "parse_comm 'inv' " >:: 
      (fun _ -> assert_equal Inventory (parse_comm "INventorY" ));
  "parse_comm 'inv'" >:: 
      (fun _ -> assert_equal Inventory (parse_comm "inv" ));
  

  "parse_comm help" >:: 
      (fun _ -> assert_equal Help (parse_comm "  HeLp  " ));
  "parse_comm help" >:: 
      (fun _ -> assert_equal Help (parse_comm "help" ));
  "parse_comm help" >:: 
      (fun _ -> assert_equal Help (parse_comm "HELP" ));

  "parse_comm Check " >:: 
      (fun _ -> assert_equal Check (parse_comm "  ChecK  " ));
  "parse_comm Check " >:: 
      (fun _ -> assert_equal Check  (parse_comm "check" ));
  "parse_comm Check " >:: 
      (fun _ -> assert_equal Check  (parse_comm "CHECK" ));

  "parse_coom error" >:: 
      (fun _ -> assert_raises (Failure "Illegal") (fun _ -> parse_comm "     " ));
  "parse_coom error" >:: 
      (fun _ -> assert_raises (Failure "Illegal") (fun _-> parse_comm  ""));


  

]


let interpret_command_error_tests = 
[

  (* completely bogus commands *)
  "interpret_command 1" >:: 
      (fun _ -> assert_raises (Controller.Illegal) (fun () ->interpret_command "take" 1234 w1));
  "interpret_command 2" >:: 
      (fun _ -> assert_raises (Controller.Illegal) (fun () -> interpret_command ";alsk ja; "  1234 w1));
  "interpret_command 3" >:: 
      (fun _ -> assert_raises (Failure "Illegal") (fun () -> interpret_command "     " 1234 w1));
  "interpret_command 4" >:: 
      (fun _ -> assert_raises (Failure "Illegal") (fun () ->interpret_command "" 1234 w1));
  "interpret_command 5" >:: 
      (fun _ -> assert_raises (Controller.Illegal) (fun () -> interpret_command "takeitem 9" 1234 w1));
  "interpret_command 6" >:: 
      (fun _ -> assert_raises (Controller.Illegal) (fun () -> interpret_command "go south" 1234 w1));
  "interpret_command 7" >:: 
      (fun _ -> assert_raises (Controller.Illegal) (fun () -> interpret_command "dropmic" 1234 w1));
  "parse_coom error" >:: 
      (fun _ -> assert_raises (Controller.Illegal) (fun () -> interpret_command "take" 1234 w1));
  "parse_coom error" >:: 
      (fun _ -> assert_raises (Controller.Illegal) (fun () -> interpret_command "go " 1234 w1));
  "parse_coom error" >:: 
      (fun _ -> assert_raises (Controller.Illegal) (fun () -> interpret_command "drop" 1234 w1));

  (* items that aren't in the world *)
 


]

let suite =
  "Fantastic test suite">::: parse_comm_tests @ interpret_command_error_tests 

let _ = run_test_tt_main suite
