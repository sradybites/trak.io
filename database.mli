(**Base module for skeleton/starter functions taking care of database 
   operations*)

(**Represents the structure for a task. Contains all the details to do with a 
   certain task. Mainly used for testing purposes.*)
type task = {id : int; assignee : string; title : string; 
             status : string; description : string}

(**Represents a team data type. Contains all the details to do with a certain 
   team. Mainly used for testing purposes.*)
type team = {team_name : string; members : string list}

(**Represents login details. Contains all the details to do with a 
   certain login. Mainly used for testing purposes.*)
type login_details = {username : string; password : string}

(** Raised when nothing was found in a search*)
exception NotFound of string

(** [search_tasks criterion] is a list containing all the tasks that contain the
    search critetion.
    Raises [NotFound] if nothing was found correspondent to the search 
    condition. *)
val search_tasks : string -> task list

(** [search_teams criterion] is a list containing all the teams that contain the
    search critetion.
    Raises [NotFound] if nothing was found correspondent to the search 
    condition. *)
val search_teams : string -> team list

(**[search_logins criterion] is a list of login_details corresponding to 
   criterion. Mainly used for testing.
   Raises [NotFound] if nothing was found correspondent to the search 
    condition. *)
val search_logins : string -> login_details list

(** [delete_task id] removes the task with id matching [id]. *)
val delete_task : int -> unit

(** [add_data_all filename data id_required] is a function that writes the
    given data into the top of file found at [filename]. This function is 
    different from [add_data] in that it can add any type of given data.*)
val add_data_all : string -> string list -> bool -> unit

(** [edit_task change field id] edits the textual representation of the 
    task data with id number [id], changing the field [field] to [change]. *)
val edit_task : string -> string -> int -> unit
