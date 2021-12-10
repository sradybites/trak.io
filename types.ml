open Cluster
open Field

type team = {teamname: string; 
             members: (string * user_access) list}
type task = {id: int; 
             assignee: string; 
             title: string; 
             status: string; 
             description: string}
type login = {username: string; password: string}

module Task : EntryType with type t = task = struct

  type t = task

  let assoc_file = "issues.txt"

  let create_entry = function
    | id::assignee::title::status::description -> 
      {id = int_of_string id; assignee = assignee; title = title; 
       status = status; 
       description =
         if List.length description = 1 && List.hd description = ""  
         then "" else
           let temp_descr = 
             List.fold_left (fun x y -> x ^ ";" ^ y) "" description in
           String.sub temp_descr 1 ((String.length temp_descr) - 1)}
    | _ -> failwith "mistake with creating a task"

  let update_field field t =
    match field with
    | `ID id -> {t with id=id}
    | `User name -> {t with assignee=name}
    | `Title title -> {t with title=title}
    | `Status status -> {t with status=status}
    | `Description desc -> {t with description=desc}
    | _ -> t

  let to_string_list t =
    [string_of_int t.id; t.assignee; t.title; t.status; t.description]

  let to_field_list t =
    [`ID t.id; `User t.assignee; `Title t.title; `Status t.status; 
     `Description t.description]
end

module Team : EntryType with type t = team = struct

  let assoc_file = "teams.txt"

  type t = team

  let make_user string =
    match (String.split_on_char ' ') string with
    | role::username::[] -> username, user_access_of_string role
    | _ -> failwith "mistake with creating a team"

  let inv_make_user (s, role) = string_of_user_access role ^ " " ^ s

  let create_entry = function 
    | h::t -> {teamname = h; members = List.map make_user t}
    | _ -> failwith "mistake with creating a team"

  let rec update_field field t =
    match field with
    | `TeamName name -> {t with teamname=name}
    | `Member m -> {t with members = m :: t.members}
    | _ -> t

  let to_string_list t = t.teamname :: List.map inv_make_user t.members

  let to_field_list t =
    `TeamName t.teamname :: List.map (fun e -> `Member e) t.members
end

module Login : EntryType with type t = login = struct
  let assoc_file = "login_details.txt"

  type t = login

  let create_entry = function 
    | u::p::[] -> {username = u; password = p}
    | _ -> failwith "mistake with creating a login"

  let update_field field t =
    match field with
    | `User name -> {t with username=name}
    | `Password pass -> {t with password=pass}
    | _ -> t

  let to_string_list t = [t.username; t.password]

  let to_field_list t = [`User t.username; `Password t.password]
end