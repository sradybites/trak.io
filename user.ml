(*Implementation of all frontend to database operations on a user*)
open MakeCluster
open Cluster
open Field
open Query

module Teams = MakeCluster (Types.Team) (NoIDSchema)
module Tasks = MakeCluster (Types.Task) (NumIDSchema)
module LoginBase = MakeCluster (Types.Login) (NoIDSchema)


(********Types********)
type user = {tasks : Types.task list; teams : Types.team list; 
             role : user_access}

type input_type = 
  | Password
  | Username

(********Types********)

(********Exceptions********)
exception User_Not_In_Team of string
exception Database_Fatal_Error of string
exception User_Already_Exists of string
(********Exceptions********)

(********General Helpers********)
let get_team_tasks (team : Types.team) = 
  let team_members = List.map (fun (user, role) -> user) team.members in
  Tasks.query (GET (FROM None, WHERE ("assignee" >>> team_members)))

let get_task_by_id (tasks : Types.task list) id = 
  List.find (fun (task : Types.task) -> task.id = id) tasks

let user_in_team username (team : Types.team) = 
  List.fold_left (fun b (name, _) -> b || (name = username)) false team.members

let append_task data tasks = 
  let id = 
    (List.hd (List.rev (Tasks.search (Sloppy, function | _ -> true)))).id in
  let sorter (t1 : Types.task) (t2 : Types.task) = 
    if t1.id > t2.id then 1 else
    if t1.id = t2.id then 0 else -1 in
  let (task : Types.task) = match data with
    | assignee :: title :: status :: description :: _ -> 
      {id = id; assignee = assignee; title = title;
       status = status; description = description} 
    | _ -> failwith "" in
  List.sort sorter (task :: tasks)

let manager_task_write assignee task_data team tasks =
  if user_in_team assignee team then
    let task_to_write = 
      assignee :: task_data in
    match Tasks.add task_to_write with
    | Ok i -> append_task task_to_write tasks
    | Error s -> raise (Database_Fatal_Error s)
  else raise (User_Not_In_Team assignee)

let manager_task_remove id tasks =
  let remover (task : Types.task) = 
    task.id != id in 
  match Tasks.delete (Sloppy, function | `ID i -> i = id | _ -> false) with
  | Ok i -> if i = 1 then List.filter remover tasks else tasks
  | Error s -> raise (Database_Fatal_Error s)

let manager_task_edit id field new_val tasks =
  let edit_task id field nv (tasks : Types.task list) = 
    List.map (fun (task : Types.task) -> if task.id = id then 
                 match field with
                 | "assignee" -> {task with assignee = nv}
                 | "title" -> {task with title = nv}
                 | "status" -> {task with status = nv}
                 | "description" -> {task with description = nv}
                 | _ -> failwith "" 
               else task) tasks in
  let fieldt = Field.make_str_field field new_val in
  match Tasks.update fieldt 
          (Sloppy, function | `ID i -> i = id
                            | _ -> false) with
  | true -> edit_task id field new_val tasks
  | false -> raise (Database_Fatal_Error "Database error")

let by_user username = 
  Strict, function | `User s -> s = username | _ -> true

let contains_user username =
  Sloppy, function | `Member (s, _) -> s = username | _ -> false

let by_teamname teamname = 
  Strict, function | `TeamName s -> s = teamname | _ -> true

(** Note that this helper operates on the assumption of one team and one role
    per user. *)
let recover_team_role username (teams : Types.team list) =
  match teams with
  | [] -> raise (User_Not_In_Team username)
  | team::_ as l -> (l, List.assoc username team.members)
(********General Helpers********)

let create_session username = 
  let tasks =
    by_user username |> Tasks.search in
  let team_match, role = 
    contains_user username |> Teams.search |> recover_team_role username in
  {tasks = tasks; teams = team_match; role = role}

let add_task_data task_data user assignee = 
  match user.role with
  | Manager -> failwith ""
  | Engineer -> failwith ""
  | Scrummer -> failwith ""

let rec check_logins (logins : Types.login list) username = 
  match logins with
  | [] -> raise (Database.NotFound username)
  | h::t -> if username = h.username then h.password
    else check_logins t username

let log_in username =
  let results = by_user username |> LoginBase.search in
  match results with 
  | logins -> check_logins logins username

let get_team teamname =
  let results = by_teamname teamname |> Teams.search in
  match results with
  | [] -> raise (Database.NotFound teamname)
  | h::[] -> h
  | _ -> failwith "More than one. Choose"

let validate_input input i_type = 
  let new_input = String.trim input in 
  let length = String.length new_input in 
  if i_type = Username && (length < 4 || length > 20) then false 
  else if i_type = Password && length < 8 then false 
  else if String.contains new_input ' ' then false 
  else if i_type = Username && 
          (Str.string_match (Str.regexp "^[a-zA-Z0-9]+$") 
             new_input 0) = false then false 
  else if i_type = Password && (String.contains new_input '\\') = true 
  then false 
  else true 

let add_user username password access (team : Types.team) = 
  let add_to_loginbase user password = 
    match LoginBase.add (user :: password :: []) with
    | Ok i -> 
      {team with members = 
                   (username, 
                    Field.user_access_of_string access) :: team.members}
    | Error s -> raise (Database_Fatal_Error s) in
  match List.find (fun (user, _) -> username = user) team.members with
  | user -> raise (User_Already_Exists username)
  | exception (Not_found) -> 
    if (Teams.update (`Member (username, Field.user_access_of_string access)) 
          (Sloppy, function | `TeamName tn -> tn = team.teamname | _ -> false))
    then add_to_loginbase username password 
    else raise (Database_Fatal_Error username)

let update_teams tml (new_team : Types.team) = 
  List.map (fun (team : Types.team) -> 
      if team.teamname = new_team.teamname then new_team else team) tml