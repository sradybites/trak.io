type user_access = 
  | Manager
  | Engineer
  | Scrummer

type t =
  [
    | `ID of int
    | `User of string
    | `Title of string
    | `Status of string
    | `Description of string
    | `TeamName of string

    | `Member of string * user_access

    | `Entry of t list
    | `Password of string
  ]

let user_access_of_string = function
  | "Manager" -> Manager
  | "Scrummer" -> Scrummer
  | "Engineer" -> Engineer
  | _ -> failwith "issue assigning a role"

let string_of_user_access = function
  | Manager -> "Manager"
  | Scrummer -> "Scrummer"
  | Engineer -> "Engineer"

let rec equal a b =
  match a, b with
  | `ID a, `ID b -> a = b
  | `User a, `User b -> a = b
  | `Title a, `Title b -> a = b
  | `Status a, `Status b -> a = b
  | `Description a, `Description b -> a = b
  | `TeamName a, `TeamName b -> a = b

  | `Member (a, b), `Member (c, d) -> a = c && b = d

  | `Entry a, `Entry b ->
    (try List.for_all2 equal a b with Invalid_argument _ -> false)
  | _ -> false

let make_str_field str (v : string) : t = 
  match str with
  | "title" -> `Title v
  | "Title" -> `Title v
  | "status" -> `Status v
  | "Status" -> `Status v
  | "description" -> `Description v
  | "Description" -> `Description v
  | "assignee" | "Assignee" | "user" | "User" -> `User v
  | _ -> failwith "role does not exist"