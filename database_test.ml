open OUnit2
open Database

(********************************************************************
   Here, you can put helper functions to test our modules. 
 ********************************************************************)
let search_tasks_test (name : string) (criteria : string) 
    (expected_output) = 
  name >:: (fun _ -> 
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (search_tasks criteria)))

let search_tasks_with_add_test (name : string) (criteria : string) 
    (add : string list) (expected_output) = 
  name >:: (fun _ -> 
      add_data "issues.txt" add;
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (search_tasks criteria)))

let search_tasks_with_delete_test (name : string) (criteria : string) 
    (add : int) (expected_output) = 
  name >:: (fun _ -> 
      delete_task add;
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (search_tasks criteria)))

let search_teams_test (name : string) (criteria : string)
    (expected_output) = 
  name >:: (fun _ -> 
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (search_teams criteria)))

let search_tasks_with_edit_test (name : string) (criteria : string) change
    field id (expected_output) = 
  name >:: (fun _ -> 
      edit_task change field id;
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (search_tasks criteria)))

(********************************************************************
   End helper functions.
 ********************************************************************)

let database_tests =
  [
    search_tasks_test "search for Andrii" "Andrii" 
      [{id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""}];
    search_tasks_test "search for o" "o" 
      [{id = 4; assignee = "Clarkson"; title = "Lecture"; status = "To do";
        description = "\"All the time\""};
       {id = 3; assignee = "Brady"; title = "Code"; status = "To do";
        description = "\"brady just wants to code\""};
       {id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = "\"natasha is tired after 3110 and just wants to sleep\""};
       {id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""}];
    search_tasks_test "search for Nat" "Nat" 
      [{id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = "\"natasha is tired after 3110 and just wants to sleep\""}];
    search_teams_test "search for potter" "Potter" 
      [{team_name = "Gryffindor"; members = [" Potter, Hermione, Ron, Ginny"]}];
    search_teams_test "search for o" "o" 
      [{team_name = "3110 heroes"; members = ["Andrii";"Brady";"Natasha"]};
       {team_name = "best profs ever"; members = ["Clarkson";"White";"Gries"]};
       {team_name = "Gryffindor";
        members = ["Potter";"Hermione";"Ron";"Ginny"]};
       {team_name = "Slytherin";
        members = ["Voldemort";"Blaze";"Draco";"Crabb";"Goyle"]}];
    search_tasks_with_add_test "adding task to sleep more" "sleep" 
      ["Gries"; "Sleep"; "In development"; "just sleep"]
      [{id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = "\"natasha is tired after 3110 and just wants to sleep\""};
       {id = 5; assignee = "Gries"; title = "Sleep"; status = "In development";
        description = "\"just sleep\""}];
    search_tasks_with_delete_test "deleting Gries task" "sleep" 5
      [{id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = "\"natasha is tired after 3110 and just wants to sleep\""}];
    search_tasks_with_edit_test "make Andrii jump" "Jump" "Jump" "title" 1
      [{id = 1; assignee = "Andrii"; title = "Jump"; status = "Done";
        description = "\"yeet yote yeeten\""}];
  ]

let suite =
  "test suite for MS1"  >::: List.flatten [
    database_tests;
  ]

let _ = run_test_tt_main suite