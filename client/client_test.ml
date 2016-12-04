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
      (fun _ -> assert_equal (Drink "item1") (parse_comm "drink item1    " ));
  "parse_coom drink item1" >:: 
      (fun _ -> assert_equal (Drink "item1") (parse_comm "     Drink item1" ));
  "parse_coom drink item1" >:: 
      (fun _ -> assert_equal (Drink "item1") (parse_comm "   DRINK   item1   "));
  "parse_coom drink item1" >:: 
      (fun _ -> assert_equal (Drink "item1") (parse_comm "    dRink   iTEm1   "));

  "parse_coom spell expelliarmus bob" >:: 
      (fun _ -> assert_equal (Drink "item1") (parse_comm "spell expelliarmus, bob" ));
  "parse_coom spell expelliarmus bob" >:: 
      (fun _ -> assert_equal (Drink "item1") (parse_comm "     spell expelliarmus, bob" ));
  "parse_coom spell expelliarmus bob" >:: 
      (fun _ -> assert_equal (Drink "item1") (parse_comm "spell expelliarmus, bob       "));
  "parse_coom spell expelliarmus bob" >:: 
      (fun _ -> assert_equal (Drink "item1") (parse_comm "   sPeLl     expelliarmus     , bOb "));

  "parse_coom spell avada kedavra bob" >:: 
      (fun _ -> assert_equal (Drink "item1") (parse_comm "spell avada kedavra, bob" ));
  "parse_coom spell avada kedavra bob" >:: 
      (fun _ -> assert_equal (Drink "item1") (parse_comm "     spell avada   kedavra, bob" ));
  "parse_coom spell avada kedavra bob" >:: 
      (fun _ -> assert_equal (Drink "item1") (parse_comm "spell    avada kedavra, bob       "));
  "parse_coom spell avada kedavra bob" >:: 
      (fun _ -> assert_equal (Drink "item1") (parse_comm "   sPeLl     avada     kedavra    , bOb "));


  (*"do 'look' j1_test" >:: 
      (fun _ -> assert_equal j1_test (do' "look" j1_test));
  "do 'look' j2_test" >:: 
      (fun _ -> assert_equal j2_test (do' "look" j2_test));
  "do 'look' mr_test" >:: 
      (fun _ -> assert_equal mr_test (do' "look" mr_test));
  (*"do 'look' gates_test" >:: 
      (fun _ -> assert_equal gates_test (do' "look" gates_test));*)

  "do 'quit' j1_test" >:: 
      (fun _ -> assert_equal j1_test (do' "quit" j1_test));
  "do 'quit' j2_test" >:: 
      (fun _ -> assert_equal j2_test (do' "quit" j2_test));
  "do 'quit' mr_test" >:: 
      (fun _ -> assert_equal mr_test (do' "quit" mr_test));
  (*"do 'quit' gates_test" >:: 
      (fun _ -> assert_equal gates_test (do' "quit" gates_test));*)

  "do 'turns' j1_test" >:: 
      (fun _ -> assert_equal j1_test (do' "turns" j1_test));
  "do 'turns' j2_test" >:: 
      (fun _ -> assert_equal j2_test (do' "turns" j2_test));
  "do 'turns' mr_test" >:: 
      (fun _ -> assert_equal mr_test (do' "turns" mr_test));
  (*"do 'turns' gates_test" >:: 
      (fun _ -> assert_equal gates_test (do' "turns" gates_test));*)

  "do 'inv' j1_test" >:: 
      (fun _ -> assert_equal j1_test (do' "INv" j1_test));
  "do 'inv' j2_test" >:: 
      (fun _ -> assert_equal j2_test (do' "INventorY" j2_test));
  "do 'inv' mr_test" >:: 
      (fun _ -> assert_equal mr_test (do' "inv" mr_test));
  (*"do 'inv' gates_test" >:: 
      (fun _ -> assert_equal gates_test (do' " inventory" gates_test));*)

  "do 'score' j1_test" >:: 
      (fun _ -> assert_equal j1_test (do' "  score  " j1_test));
  "do 'score' j2_test" >:: 
      (fun _ -> assert_equal j2_test (do' "sCorE " j2_test));
  "do 'score' mr_test" >:: 
      (fun _ -> assert_equal mr_test (do' "score" mr_test));
  (*"do 'score' gates_test" >:: 
      (fun _ -> assert_equal gates_test (do' "  SCORE" gates_test));*)*)

  (* checks if it raises the right errors *)
  "parse_coom error" >:: 
      (fun _ -> assert_raises (Failure "Illegal") (fun _ -> parse_comm "take" ));
  "parse_coom error" >:: 
      (fun _ -> assert_raises (Failure "Illegal") (fun _ -> parse_comm ";alsk ja; " ));
  "parse_coom error" >:: 
      (fun _ -> assert_raises (Failure "Illegal") (fun _ -> parse_comm "     " ));
  "parse_coom error" >:: 
      (fun _ -> assert_raises (Failure "Illegal") (fun _-> parse_comm ""));
  "parse_coom error" >:: 
      (fun _ -> assert_raises (Failure "Illegal") (fun _ -> parse_comm "takeitem 9" ));
  "parse_coom error" >:: 
      (fun _ -> assert_raises (Failure "Illegal") (fun _ -> parse_comm "go south" ));
  "parse_coom error" >:: 
      (fun _ -> assert_raises (Failure "Illegal") (fun _ -> parse_comm "dropmic" ));
  "parse_coom error" >:: 
      (fun _ -> assert_raises (Failure "Illegal") (fun _ -> parse_comm "take" ));
  "parse_coom error" >:: 
      (fun _ -> assert_raises (Failure "Illegal") (fun _ -> parse_comm "go " ));
  "parse_coom error" >:: 
      (fun _ -> assert_raises (Failure "Illegal") (fun _ -> parse_comm "drop" ));
 

]
