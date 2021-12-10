(**TEST PLAN:
   1) All backend functions are tested via OUnit. Cluster is tested for Tasks, 
   Teams and Login Details. TDD was used throughtout the entire development, so
   original skeleton database functions were also tested. Additionally, 
   everything else was tested manually via the REPL front-end, since all 
   functions are used in one way or the other to handle the operation of the 
   REPL.
   2) Modules tested by OUnit and how: 
    Cluster: tested using the Task functor. All operations used in the front-end
    were tested via blackbox testing based on signatures and expected outcomes.
    User: Backend functions inside User module were tested by glass and black 
    box  testing. All OUnit tests were reviewed by all 3 of us ensuring coverage
    of all directly user functions, as well as those having an effect on data
    files. 
    Database: as the original sceleton functions for database operations these
    were tested by the entire team using Glass and Black Box. All tests 
    reviewed by the 3 members of the team. 
    Types, Field, MakeCluster: Tested via tests on Cluster since all those 
    modules are used within it.
   3) The testing approach demonstrates the correctness since all the functions 
   that are called within the Main front-end are tested with all the corner 
   cases, including exceptions and possible types of input data. Throughout the 
   development all functions were tested that had an effect and were used, 
   following the Requires and Raises rules in comments as well. 
   Test-Driven Development and testing of most possible flows through the code 
   ensured the correctness of the expected functionality.*)

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
      add_data_all "issues.txt" add true;
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

let login_test (name : string) (user: string) (err_exp : bool) expected_out = 
  name >:: (fun _ -> 
      if err_exp then
        assert_raises (Database.NotFound name) (fun () -> User.log_in name)
      else 
        assert_equal expected_out (User.log_in user))


(********************************************************************
   End helper functions.
 ********************************************************************)

let database_tests =
  [
    search_tasks_test "search for Andrii" "Andrii" 
      [{Database.id = 5; assignee = "Andrii"; title = "Get into a new college";
        status = "To do"; description = "\"New CIS college ey\""};
       {Database.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""}];
    search_tasks_test "search for o" "o" 
      [{Database.id = 5; assignee = "Andrii"; title = "Get into a new college";
        status = "To do"; description = "\"New CIS college ey\""};
       {Database.id = 4; 
        assignee = "Clarkson"; title = "Lecture"; status = "To do";
        description = "\"All the time\""};
       {Database.id = 3; assignee = "Brady"; title = "Code"; status = "To do";
        description = "\"brady just wants to code\""};
       {Database.id = 2; 
        assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = 
          "\"natasha is tired after 3110 and just wants to sleep\""};
       {Database.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""}];
    search_tasks_test "search for Nat" "Nat" 
      [{id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = 
          "\"natasha is tired after 3110 and just wants to sleep\""}];
    search_teams_test "search for potter" "Potter" 
      [{Database.team_name = "Gryffindor";
        members = ["Manager Potter"; "Scrummer Hermione"; 
                   "Engineer Ron"; "Engineer Ginny"]}];
    search_teams_test "teams search for o" "o" 
      [{team_name = "3110 heroes";
        members = ["Manager Andrii"; "Manager Brady"; "Manager Natasha"]};
       {team_name = "best profs ever";
        members = ["Scrummer Clarkson"; "Engineer White"; "Manager Gries"]};
       {team_name = "Gryffindor";
        members =
          ["Manager Potter"; "Scrummer Hermione"; 
           "Engineer Ron"; "Engineer Ginny"]};
       {team_name = "Slytherin";
        members =
          ["Manager Salazar"; "Manager Voldemort"; 
           "Engineer Blaze"; "Scrummer Draco";
           "Engineer Crabb"; "Engineer Goyle"]}];
    search_tasks_with_add_test "adding task to sleep more" "sleep" 
      ["Gries"; "Sleep"; "In development"; "\"just sleep\""]
      [{Database.id = 6; assignee = "Gries"; title = "Sleep";
        status = "In development"; description = "\"just sleep\""};
       {Database.id = 2; 
        assignee = "Natasha"; title = "Sleep"; status = "Active";
        description =
          "\"natasha is tired after 3110 and just wants to sleep\""}];
    search_tasks_with_delete_test "deleting Gries task" "sleep" 6
      [{id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = 
          "\"natasha is tired after 3110 and just wants to sleep\""}];
    search_tasks_with_edit_test "make Andrii jump" "Jump" "Jump" "title" 1
      [{id = 1; assignee = "Andrii"; title = "Jump"; status = "Done";
        description = "\"yeet yote yeeten\""}];
    search_tasks_with_edit_test "make Andrii yeet again" "Yeet" "Yeet" "title" 1
      [{id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""}];
  ]

let create_session_test name username expected_user = 
  name >:: (fun _ -> 
      assert_equal expected_user (User.create_session username))

let get_team_tasks_test name (team : Types.team) expected_tasks = 
  name >:: (fun _ -> 
      assert_equal expected_tasks (User.get_team_tasks team))

let get_task_by_id_test name tasks id exp_task exc_exp = 
  name >:: (fun _ -> 
      if exc_exp then
        assert_raises (Not_found) (fun () -> User.get_task_by_id tasks id)
      else 
        assert_equal exp_task (User.get_task_by_id tasks id))

let get_team_test name team_name exp_team exc_exp = 
  name >:: (fun _ -> 
      if exc_exp then
        assert_raises (NotFound team_name) (fun () -> User.get_team team_name)
      else 
        assert_equal exp_team (User.get_team team_name))

let man_task_write_test name username data team tasks expected_tasks 
    expected_search = 
  name >:: (fun _ -> 
      let x = User.manager_task_write username data team tasks and
      y = search_tasks username in
      assert_equal (expected_tasks, expected_search) 
        (x, y))

let man_task_edit_test name id field value user tasks expected_tasks 
    expected_search = 
  name >:: (fun _ -> 
      let x = User.manager_task_edit id field value tasks and
      y = search_tasks user in
      assert_equal (expected_tasks, expected_search) 
        (x, y))

let man_task_rem_test name id tasks crit expected_tasks expected_search = 
  name >:: (fun _ -> 
      let _ = User.manager_task_remove id tasks in
      assert_raises (Database.NotFound crit) (fun () -> search_tasks crit))

let backend_tests = 
  [
    login_test "existing user" "test" false "test12345";
    login_test "nonexisting" "nope" true "lol";
    login_test "natasha" "Natasha" false "passwordlol";
    login_test "nonexisting user within a team" "Salazar" true "slytherin";
    create_session_test "Natasha user" "Natasha"  
      {User.tasks =
         [{Types.id = 2; assignee = "Natasha"; 
           title = "Sleep"; status = "Active";
           description = 
             "\"natasha is tired after 3110 and just wants to sleep\""}];
       teams =
         [{Types.teamname = "3110 heroes";
           members =
             [("Andrii", Field.Manager); ("Brady", Field.Manager);
              ("Natasha", Field.Manager)]}];
       role = Field.Manager};
    create_session_test "Andrii two tasks" "Andrii"
      {User.tasks =
         [{Types.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
           description = "\"yeet yote yeeten\""};
          {Types.id = 5; assignee = "Andrii"; title = "Get into a new college";
           status = "To do"; description = "\"New CIS college ey\""}];
       teams =
         [{Types.teamname = "3110 heroes";
           members =
             [("Andrii", Field.Manager); ("Brady", Field.Manager);
              ("Natasha", Field.Manager)]}];
       role = Field.Manager};
    get_team_tasks_test "3110 heroes" 
      {Types.teamname = "3110 heroes"; 
       members =
         [("Andrii", Field.Manager); ("Brady", Field.Manager);
          ("Natasha", Field.Manager)]} 
      [{Types.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""};
       {Types.id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = 
          "\"natasha is tired after 3110 and just wants to sleep\""};
       {Types.id = 3; assignee = "Brady"; title = "Code"; status = "To do";
        description = "\"brady just wants to code\""};
       {Types.id = 5; assignee = "Andrii"; title = "Get into a new college";
        status = "To do"; description = "\"New CIS college ey\""}];
    get_task_by_id_test "task with id 3" 
      [{Types.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""};
       {Types.id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = 
          "\"natasha is tired after 3110 and just wants to sleep\""};
       {Types.id = 3; assignee = "Brady"; title = "Code"; status = "To do";
        description = "\"brady just wants to code\""};
       {Types.id = 5; assignee = "Andrii"; title = "Get into a new college";
        status = "To do"; description = "\"New CIS college ey\""}] 3 
      {Types.id = 3; assignee = "Brady"; title = "Code"; status = "To do";
       description = "\"brady just wants to code\""} false;
    get_task_by_id_test "error" [] 2 
      {Types.id = 3; assignee = "Brady"; title = "Code"; status = "To do";
       description = "\"brady just wants to code\""} true;
    get_team_test "non existent" "nah" 
      {Types.teamname = "3110 heroes"; members = []} true;
    get_team_test "Gryffindor people" "Gryffindor" 
      {Types.teamname = "Gryffindor";  
       members =
         [("Potter", Field.Manager); ("Hermione", Field.Scrummer);
          ("Ron", Field.Engineer); ("Ginny", Field.Engineer)]} false;
    man_task_write_test "Andrii add task" "Andrii" 
      ["Swim"; "Active"; "\"Just Swim Bro\""] 
      {Types.teamname = "3110 heroes"; 
       members =  [("Andrii", Field.Manager)]} [] 
      [{Types.id = 6; assignee = "Andrii"; title = "Swim"; status = "Active";
        description = "\"Just Swim Bro\""}] 
      [{Database.id = 6; assignee = "Andrii"; title = "Swim"; status = "Active";
        description = "\"Just Swim Bro\""};
       {Database.id = 5; assignee = "Andrii"; title = "Get into a new college";
        status = "To do"; description = "\"New CIS college ey\""};
       {Database.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""}];
    man_task_edit_test "Edit swim to fly" 6 "title" "Fly" "Andrii"
      [{Types.id = 6; assignee = "Andrii"; title = "Fly"; status = "Active";
        description = "\"Just Swim Bro\""}] 
      [{Types.id = 6; assignee = "Andrii"; title = "Fly"; status = "Active";
        description = "\"Just Swim Bro\""}]
      [{Database.id = 6; assignee = "Andrii"; title = "Fly"; status = "Active";
        description = "\"Just Swim Bro\""};
       {Database.id = 5; assignee = "Andrii"; title = "Get into a new college";
        status = "To do"; description = "\"New CIS college ey\""};
       {Database.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""}];
    man_task_rem_test "Delete the flying" 6 
      [{Types.id = 6; assignee = "Andrii"; title = "Fly"; status = "Active";
        description = "\"Just Swim Bro\""}] "Fly" [] [];
  ]

module TaskCluster = MakeCluster.MakeCluster(Types.Task)(Cluster.NumIDSchema)

let checker (qfunc : Field.t) : bool = 
  if qfunc = `User "lol" then true else false

let cluster_search_tasks_test (name : string) criteria
    (expected_output) = 
  name >:: (fun _ -> 
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (TaskCluster.search criteria)))

let cluster_search_tasks_with_add_test (name : string) criteria 
    (add : string list) (expected_output) = 
  name >:: (fun _ -> 
      let _ = TaskCluster.add add in
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (TaskCluster.search criteria)))

let cluster_search_tasks_with_delete_test (name : string) criteria 
    del_cond (expected_output) = 
  name >:: (fun _ -> 
      let _ = TaskCluster.delete del_cond in
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (TaskCluster.search criteria)))

let cluster_search_teams_test (name : string) (criteria : string)
    (expected_output) = 
  name >:: (fun _ -> 
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (search_teams criteria)))

let cluster_search_tasks_with_edit_test (name : string) criteria change tasks
    field id (expected_output) = 
  name >:: (fun _ -> 
      let _ = User.manager_task_edit id field change tasks in
      assert_equal (List.sort_uniq compare expected_output) (
        List.sort_uniq compare (TaskCluster.search criteria)))

let string_contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true

let cluster_task_tests = 
  [
    cluster_search_tasks_test "Natasha task search" 
      (Sloppy, function | `User name -> name = "Natasha" | _ -> false)
      [{Types.id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = 
          "\"natasha is tired after 3110 and just wants to sleep\""}];
    cluster_search_tasks_test "tasks with description containing s" 
      (Sloppy, function | 
          `Description descr -> String.contains descr 's' | _ -> false)
      [{Types.id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = 
          "\"natasha is tired after 3110 and just wants to sleep\""};
       {Types.id = 3; assignee = "Brady"; title = "Code"; status = "To do";
        description = "\"brady just wants to code\""}];
    cluster_search_tasks_test "tasks with id 2" 
      (Sloppy, function | `ID id -> id = 2 | _ -> false)
      [{Types.id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = 
          "\"natasha is tired after 3110 and just wants to sleep\""}];
    cluster_search_tasks_test "tasks with negative id" 
      (Sloppy, function | `ID id -> id = -2 | _ -> false) [];
    cluster_search_tasks_test "Tasks with titles containing o"
      (Sloppy, function | 
          `Title title -> String.contains title 'o' | _ -> false)
      [{Types.id = 3; assignee = "Brady"; title = "Code"; status = "To do";
        description = "\"brady just wants to code\""};
       {Types.id = 5; assignee = "Andrii"; title = "Get into a new college";
        status = "To do"; description = "\"New CIS college ey\""}];
    cluster_search_tasks_test "Tasks with status containing o"
      (Sloppy, function | `Status sts -> String.contains sts 'o' | _ -> false)
      [{Types.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""};
       {Types.id = 3; assignee = "Brady"; title = "Code"; status = "To do";
        description = "\"brady just wants to code\""};
       {Types.id = 4; 
        assignee = "Clarkson"; title = "Lecture"; status = "To do";
        description = "\"All the time\""};
       {Types.id = 5; assignee = "Andrii"; title = "Get into a new college";
        status = "To do"; description = "\"New CIS college ey\""}];
    cluster_search_tasks_test "Tasks where descriptions contain the word yeet"
      (Sloppy, function 
          | `Description dscr -> string_contains dscr "yeet" | _ -> false)
      [{Types.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""}];
    cluster_search_tasks_with_add_test "Add another yeet task"
      (Sloppy, function 
          | `Description dscr -> string_contains dscr "yeet" | _ -> false)
      ["Clarkson"; "Grade exams"; "Active"; "yeet done"] 
      [{Types.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""};
       {Types.id = 6; assignee = "Clarkson"; title = "Grade exams";
        status = "Active"; description = "yeet done"}];
    cluster_search_tasks_with_add_test "Add another task for Natasha" 
      (Sloppy, function | `User name -> name = "Natasha" | _ -> false)
      ["Natasha"; "Finish the final project"; "Done"; "YAAAAAY"]
      [{Types.id = 2; assignee = "Natasha"; title = "Sleep"; status = "Active";
        description = 
          "\"natasha is tired after 3110 and just wants to sleep\""};
       {Types.id = 7; assignee = "Natasha"; title = "Finish the final project";
        status = "Done"; description = "YAAAAAY"}];
    cluster_search_tasks_with_add_test 
      "Add another task for status containing o" 
      (Sloppy, function | `Status sts -> String.contains sts 'o' | _ -> false)
      ["Brady"; "Finish school"; "Done"; "YAAAAAY"]
      [{Types.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""};
       {Types.id = 3; assignee = "Brady"; title = "Code"; status = "To do";
        description = "\"brady just wants to code\""};
       {Types.id = 4; 
        assignee = "Clarkson"; title = "Lecture"; status = "To do";
        description = "\"All the time\""};
       {Types.id = 5; assignee = "Andrii"; title = "Get into a new college";
        status = "To do"; description = "\"New CIS college ey\""};
       {Types.id = 7; assignee = "Natasha"; title = "Finish the final project";
        status = "Done"; description = "YAAAAAY"};
       {Types.id = 8; assignee = "Brady"; 
        title = "Finish school"; status = "Done";
        description = "YAAAAAY"}];
    cluster_search_tasks_with_add_test "Add another task for Andrii" 
      (Sloppy, function | `User name -> name = "Andrii" | _ -> false)
      ["Andrii"; "Finish CS3110"; "Almost"; "YAAAAAY"]
      [{Types.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""};
       {Types.id = 5; assignee = "Andrii"; title = "Get into a new college";
        status = "To do"; description = "\"New CIS college ey\""};
       {Types.id = 9; assignee = "Andrii"; title = "Finish CS3110";
        status = "Almost"; description = "YAAAAAY"}];
    cluster_search_tasks_with_edit_test "Edit first task to Gries"
      (Sloppy, function | `User name -> name = "Gries" | _ -> false)
      "Gries" [] "Assignee" 1 
      [{Types.id = 1; assignee = "Gries"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""}];
    cluster_search_tasks_with_edit_test "Edit first task back to Andrii" 
      (Sloppy, function | `User name -> name = "Andrii" | _ -> false)
      "Andrii" [] "assignee" 1 
      [{Types.id = 1; assignee = "Andrii"; title = "Yeet"; status = "Done";
        description = "\"yeet yote yeeten\""};
       {Types.id = 5; assignee = "Andrii"; title = "Get into a new college";
        status = "To do"; description = "\"New CIS college ey\""};
       {Types.id = 9; assignee = "Andrii"; title = "Finish CS3110";
        status = "Almost"; description = "YAAAAAY"}];
    cluster_search_tasks_with_delete_test "Delete id 9"
      (Sloppy, function | `ID id -> id = 9 | _ -> false)
      (Sloppy, function | `ID id -> id = 9 | _ -> false) [];
    cluster_search_tasks_with_delete_test "Delete id 8"
      (Sloppy, function | `ID id -> id = 8 | _ -> false)
      (Sloppy, function | `ID id -> id = 8 | _ -> false) [];
    cluster_search_tasks_with_delete_test "Delete id 7"
      (Sloppy, function | `ID id -> id = 7 | _ -> false)
      (Sloppy, function | `ID id -> id = 7 | _ -> false) [];
    cluster_search_tasks_with_delete_test "Delete id 6"
      (Sloppy, function | `ID id -> id = 6 | _ -> false)
      (Sloppy, function | `ID id -> id = 6 | _ -> false) [];
  ]

let validate_input_test name inp itype expected_out = 
  name >:: (fun _ -> 
      assert_equal expected_out (User.validate_input inp itype))

let main_funcs_tests = 
  [
    validate_input_test "simple username" "Andrii" User.Username true;
    validate_input_test "simple password" "12345678910" User.Password true;
    validate_input_test "password too short" "123" User.Password false;
    validate_input_test "username too short" "and" User.Username false;
    validate_input_test "username too long" "NatashaNatashaNatashaNatashaNatash"
      User.Username false;
    validate_input_test "username has special chars" "Andrii+" User.Username 
      false;
    validate_input_test "password has backslash" "hi\\135262367" User.Password
      false;
    validate_input_test "valid username" "Natasha" User.Username true;
    validate_input_test "valid password" "helloworld12345" User.Password true;
    validate_input_test "valid password" "qwertyforever" User.Password true;
    validate_input_test "valid username" "Brady" User.Username true;
    validate_input_test "empty username" "" User.Username false;
  ]

let suite =
  "test suite for MS1"  >::: List.flatten [
    backend_tests;
    database_tests;
    cluster_task_tests;
    main_funcs_tests;
  ]

let _ = run_test_tt_main suite