(**All the backend functions needed for the Front-End.*)
open Field

(**Represents when the needed user is not in the given team.*)
exception User_Not_In_Team of string

(**Represents a fatal error in the database.*)
exception Database_Fatal_Error of string

(**Represents when a user already exists and can't be created therefore.*)
exception User_Already_Exists of string

(**Represents a given user with all the data associated with the 
   current session.*)
type user = {tasks : Types.task list; teams : Types.team list; 
             role : user_access}

(**[log_in username] is a string that indicates the password that is expected 
   if the user exists. 
   Raises: NotFound if the user doesnt exist*)
val log_in : string -> string

(**[create_session username] is a user representing the session associated with 
   a logged in user with username username.
   Requires: username is an existing username*)
val create_session : string -> user

(**[manager_task_write username data team tasks] writes data into the file and 
   associates it to the user username. Returns the new task list containing the
   added task to verify that the correct Database function was used. 
   Requires: data is of a correct format corresponding to the team.
   Raises: Database_Fatal_Error code if some database error has occured. code 
   indicates the error.*)
val manager_task_write : string -> string list -> Types.team -> 
  Types.task list -> Types.task list

(**[manager_task_remove id tasks] is the task list with task with ID id removed.
   The corresponding task is also removed from the task file. If no task with ID
   id is present than the task list is unmodified. 
   Raises: Database_Fatal_Error code if some database error has occured. code 
   indicates the error.*)
val manager_task_remove : int -> Types.task list -> Types.task list

(**[manager_task_edit id field value tasks] is the task list with the task with 
   ID id edited in field field to value value. The corresponding file is also 
   altered.
   Requires: Field is a valid field.
   Raises: Database_Fatal_Error code if some database error has occured. code 
   indicates the error.*)
val manager_task_edit : int -> string -> string 
  -> Types.task list -> Types.task list

(**[get_team_tasks team] is the list of tasks associated with all the members of 
   the team.*)
val get_team_tasks : Types.team -> Types.task list

(**[get_task_by_id tasks id] is the task with id id in tasks
   Raises: Not_Found if no task with id id exists in the list.*)
val get_task_by_id : Types.task list -> int -> Types.task

(**[get_team teamname] returns the team type associated with the given 
   teamname. 
   Raises: Database.NotFound if the teamname does not exist. *)
val get_team : string -> Types.team

(** [input_type] is the type representing either a username or password to be
    used when validating strings. *)
type input_type = 
  | Password
  | Username

(** [validate_input input i_type] validates a given string based on its 
    input_type (username or password). 
    Restrictions include: username must be between 4 and 20 chars, password no 
    smaller than 8 chars. Usernames cannot contain special characters, 
    but passwords can (except backslash). *)
val validate_input : string -> input_type -> bool

(**[add_user username password access team] adds the user with username username
   with password password and role access to the team team. After updating all 
   required files returns the team with the user added.
   Requires: username and password are valid according to validation.
   Raises: User_Already_Exists username if the user already exists.
   Database_Fatal_error s with s indicating the error that happened.*)
val add_user : string -> string -> string -> Types.team -> Types.team

(**[update_teams old_team_list new_team] is the old_team_list with the old team
   team update to new_team.*)
val update_teams : Types.team list -> Types.team -> Types.team list