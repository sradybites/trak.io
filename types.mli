(** Types that are used within the program, e.g. for fields. *)

open Cluster
open Field

(**Represents a team data type. Contains all the details to do with a certain 
   team.*)
type team = {teamname: string; members: (string * user_access) list}

(**Represents the structure for a task. Contains all the details to do with a 
   certain task.*)
type task = {id: int; 
             assignee: string; 
             title: string; 
             status: string; 
             description: string}

(**Represents login details. Contains all the details to do with a 
   certain login.*)
type login = {username: string; password: string}

(**Module corresponding to Tasks*)
module Task : EntryType with type t = task

(**Module corresponding to Teams.*)
module Team : EntryType with type t = team

(**Module corresponding to Logins.*)
module Login : EntryType with type t = login