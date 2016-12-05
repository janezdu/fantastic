open OUnit2
open Model
open Clienthttp
open Controller
open Cli

let j1 = Yojson.Basic.from_file "../worlds/fourrooms.json" 
let w1 = Controller.init_state j1

let j2 = Yojson.Basic.from_file "../worlds/test4rooms.json"
let w2 = Controller.init_state j2

let diff_conversion_tests =
[

]

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
      (fun _ -> assert_raises (Controller.Illegal) 
          (fun () ->interpret_command "take" 1234 w1));
  "interpret_command 2" >:: 
      (fun _ -> assert_raises (Controller.Illegal) 
          (fun () -> interpret_command ";alsk ja; "  1234 w1));
  "interpret_command 3" >:: 
      (fun _ -> assert_raises (Failure "Illegal") 
          (fun () -> interpret_command "     " 1234 w1));
  "interpret_command 4" >:: 
      (fun _ -> assert_raises (Failure "Illegal") 
          (fun () ->interpret_command "" 1234 w1));
  "interpret_command 5" >:: 
      (fun _ -> assert_raises (Controller.Illegal) 
          (fun () -> interpret_command "takeitem 9" 1234 w1));
  "interpret_command 6" >:: 
      (fun _ -> assert_raises (Controller.Illegal) 
          (fun () -> interpret_command "go south" 1234 w1));
  "interpret_command 7" >:: 
      (fun _ -> assert_raises (Controller.Illegal) 
          (fun () -> interpret_command "dropmic" 1234 w1));
  "parse_coom error" >:: 
      (fun _ -> assert_raises (Controller.Illegal) 
          (fun () -> interpret_command "take" 1234 w1));
  "parse_coom error" >:: 
      (fun _ -> assert_raises (Controller.Illegal) 
          (fun () -> interpret_command "go " 1234 w1));
  "parse_coom error" >:: 
      (fun _ -> assert_raises (Controller.Illegal) 
          (fun () -> interpret_command "drop" 1234 w1));

  (* items that aren't in the world, invalid directions *)
  "interpret_command 1" >:: 
      (fun _ -> assert_raises (Controller.NotAnItem) 
          (fun () ->interpret_command "take finite incantatum" 1234 w1));
  "interpret_command 2" >:: 
      (fun _ -> assert_raises (Controller.NotAnItem) 
          (fun () -> interpret_command "spell revelio, rebecca "  1234 w1));
  "interpret_command 3" >:: 
      (fun _ -> assert_raises (Failure "Illegal") 
          (fun () -> interpret_command "     " 1234 w1));
  "interpret_command 4" >:: 
      (fun _ -> assert_raises (Failure "Illegal") 
          (fun () ->interpret_command "" 1234 w1));
  "interpret_command 5" >:: 
      (fun _ -> assert_raises (Controller.NotAnItem) 
          (fun () -> interpret_command "take 9" 1234 w1));
  "interpret_command 6" >:: 
      (fun _ -> assert_raises (Controller.Illegal) 
          (fun () -> interpret_command "move left" 1234 w1));
  "interpret_command 7" >:: 
      (fun _ -> assert_raises (Controller.NotAnItem) 
          (fun () -> interpret_command "drop pusheen" 1234 w1));
  "interpret_command 8" >:: 
      (fun _ -> assert_raises (Controller.NotAnItem) 
          (fun () -> interpret_command "take felix felices" 1234 w1));
  "nterpret_command  9" >:: 
      (fun _ -> assert_raises (Controller.Illegal) 
          (fun () -> interpret_command "up" 1234 w1));
  "nterpret_command  10" >:: 
      (fun _ -> assert_raises (Controller.NotAnItem) 
          (fun () -> interpret_command "drop gillyweed" 1234 w1));
  "nterpret_command  11" >:: 
      (fun _ -> assert_raises (Controller.Illegal) 
          (fun () -> interpret_command "quit game" 1234 w1));
 "interpret_command spell but not player" >:: 
      (fun _ -> assert_raises (Controller.NotAnItem) 
          (fun () -> interpret_command "spell lumos, tom "  1234 w1));
  "interpret_command spell neither" >:: 
      (fun _ -> assert_raises (Controller.NotAnItem) 
          (fun () -> interpret_command "spell revelio, tom "  1234 w1));

]

let interpret_command_tests =
[
  "interpret_command good 1" >:: 
      (fun _ -> assert_equal (JMove "{\"new_x\":1, \"new_y\": 0}") 
          (interpret_command "move north" 1000 w2));
  "interpret_command good 1" >:: 
      (fun _ -> assert_equal (JMove "{\"new_x\":0, \"new_y\": 1}") 
          (interpret_command "move south" 1234 w2));
  "interpret_command good 1" >:: 
      (fun _ -> assert_equal (JMove "{\"new_x\":0, \"new_y\": 1}") 
          (interpret_command "move east" 1000 w2));
  "interpret_command good 1" >:: 
      (fun _ -> assert_equal (JMove "{\"new_x\":1, \"new_y\": 0}") 
          (interpret_command "move west" 1234 w2));
  "interpret_command good 1" >:: 
      (fun _ -> assert_equal (JMove "{\"new_x\":0, \"new_y\": 1}") 
          (interpret_command "move north" 1234 w2));
  "interpret_command good 1" >:: 
      (fun _ -> assert_equal (JMove "{\"new_x\":1, \"new_y\": 0}") 
          (interpret_command "move south" 1000 w2));

  "interpret_command good 2" >:: 
      (fun _ -> assert_equal (JTake "{\"id\":1}") 
          (interpret_command "take lumos" 1234 w1));
  "interpret_command good 2" >:: 
      (fun _ -> assert_equal (JTake "{\"id\":2}") 
          (interpret_command "take avada kedavra" 1234 w2));
  "interpret_command good 2" >:: 
      (fun _ -> assert_equal (JTake "{\"id\":3}") 
          (interpret_command "take pepperup potion" 1234 w2));
  "interpret_command good 2" >:: 
      (fun _ -> assert_equal (JTake "{\"id\":1}") 
          (interpret_command "take lumos" 1000 w1));
  "interpret_command good 2" >:: 
      (fun _ -> assert_equal (JTake "{\"id\":2}") 
          (interpret_command "take avada kedavra" 1000 w2));
  "interpret_command good 2" >:: 
      (fun _ -> assert_equal (JTake "{\"id\":3}") 
          (interpret_command "take pepperup potion" 1000 w2));

  "interpret_command good 3" >:: 

      (fun _ -> assert_equal (JSpell "{\"id\":2, \"target\":1234}") 
          (interpret_command "spell avada kedavra, rebecca" 1000 w2));
  "interpret_command good 4" >:: 
      (fun _ -> assert_equal (JSpell "{\"id\":1, \"target\":1000}") 
          (interpret_command "spell lumos, bob" 1000 w2));
  "interpret_command good 4" >:: 
      (fun _ -> assert_equal (JSpell "{\"id\":1, \"target\":1000}") 
          (interpret_command "spell lumos, bob" 1234 w2));
  "interpret_command good 4: isn't right but no errors" >:: 
      (fun _ -> assert_equal (JSpell "{\"id\":3, \"target\":1234}") 
          (interpret_command "spell pepperup potion, rebecca" 1234 w2));
  "interpret_command good 4: isn't right but no errors" >:: 
      (fun _ -> assert_equal (JSpell "{\"id\":3, \"target\":1234}") 
          (interpret_command "spell pepperup   potion    ,  rebecca " 1234 w2));
  "interpret_command good 4: isn't right but no errors" >:: 
      (fun _ -> assert_equal (JSpell "{\"id\":3, \"target\":1000}") 

          (interpret_command "spell pepperup potion, bob" 1234 w2));

  "interpet_command good drop 1">::
      (fun _ -> assert_equal (JDrop "{\"id\":1}") 
          (interpret_command "drop lumos" 1234 w2));
  "interpet_command good drop 1">::
      (fun _ -> assert_equal (JDrop "{\"id\":2}") 
          (interpret_command "drop avada kedavra" 1234 w2));
  "interpet_command good drop 1">::
      (fun _ -> assert_equal (JDrop "{\"id\":3}") 
          (interpret_command "drop pepperup potion" 1234 w2));
  "interpet_command good drop 1">::
      (fun _ -> assert_equal (JDrop "{\"id\":1}") 
          (interpret_command "drop lumos" 1000 w2));
  "interpet_command good drop 1">::
      (fun _ -> assert_equal (JDrop "{\"id\":2}") 
          (interpret_command "drop avada kedavra" 1000 w2));
  "interpet_command good drop 1">::
      (fun _ -> assert_equal (JDrop "{\"id\":3}") 
          (interpret_command "drop pepperup potion" 1000 w2));

  (* drink, look, view, inv, help, check, quit*)
  (* drink *)
  "interpet_command good drink 1">::
      (fun _ -> assert_equal (JDrink "{\"id\":3, \"target\":1000}") 
          (interpret_command "drink pepperup potion" 1000 w2));
  "interpet_command good drink 1">::
      (fun _ -> assert_equal (JDrink "{\"id\":3, \"target\":1234}") 
          (interpret_command "drink pepperup potion" 1234 w2));
  "interpet_command good drink 1">::
      (fun _ -> assert_equal (JDrink "{\"id\":1, \"target\":1000}") 
          (interpret_command "drink lumos" 1000 w2));
  "interpet_command good drink 2">::
      (fun _ -> assert_equal (JDrink "{\"id\":2, \"target\":1000}") 
          (interpret_command "drink avada kedavra" 1000 w2));

  (* look *)
  "interpet_command good look 1">::
      (fun _ -> assert_equal (JLook) 
          (interpret_command "look" 1000 w2));
  "interpet_command good look 2">::
      (fun _ -> assert_equal (JLook) 
          (interpret_command "LooK     " 1234 w2));
  "interpet_command good look 3">::
      (fun _ -> assert_equal (JLook) 
          (interpret_command "   LOOK     " 1234 w2));

  (* view *)
  "interpet_command good view 1">::
      (fun _ -> assert_equal (JViewState) 
          (interpret_command "view" 1000 w2));
  "interpet_command good view 2">::
      (fun _ -> assert_equal (JViewState) 
          (interpret_command "VieW     " 1234 w2));
  "interpet_command good view 3">::
      (fun _ -> assert_equal (JViewState) 
          (interpret_command "   VIEW     " 1234 w2));

  (* inv *)
  "interpet_command good inv 1">::
      (fun _ -> assert_equal (JInv) 
          (interpret_command "inv" 1000 w2));
  "interpet_command good inv 2">::
      (fun _ -> assert_equal (JInv) 
          (interpret_command "inventory     " 1234 w2));
  "interpet_command good inv 2">::
      (fun _ -> assert_equal (JInv) 
          (interpret_command "   InventoRy     " 1234 w2));

  (* help *)
  "interpet_command good help 1">::
      (fun _ -> assert_equal (JHelp) 
          (interpret_command "help" 1000 w2));
  "interpet_command good help 2">::
      (fun _ -> assert_equal (JHelp) 
          (interpret_command "HELP     " 1234 w2));
  "interpet_command good help 3">::
      (fun _ -> assert_equal (JHelp) 
          (interpret_command "   HeLP     " 1234 w2));

  (* check *)
  "interpet_command good check 1">::
      (fun _ -> assert_equal (JCheck) 
          (interpret_command "check" 1000 w2));
  "interpet_command good check 2">::
      (fun _ -> assert_equal (JCheck) 
          (interpret_command " CHeck    " 1234 w2));
  "interpet_command good check 3">::
      (fun _ -> assert_equal (JCheck) 
          (interpret_command "   CHECK    " 1234 w2));

  (* quit *)
  "interpet_command good quit 1">::
      (fun _ -> assert_equal (JQuit) 
          (interpret_command "quit" 1000 w2));
  "interpet_command good quit 2">::
      (fun _ -> assert_equal (JQuit) 
          (interpret_command "QuiT     " 1234 w2));
  "interpet_command good quit 3">::
      (fun _ -> assert_equal (JQuit) 
          (interpret_command "   QUIT    " 1234 w2));


 
]



let suite =
  "Fantastic test suite">::: parse_comm_tests @ interpret_command_error_tests
  @ interpret_command_tests 


let _ = run_test_tt_main suite

