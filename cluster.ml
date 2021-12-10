module type EntryType = sig
  type t
  val assoc_file : string
  val create_entry : string list -> t
  val update_field : Field.t -> t -> t
  val to_string_list : t -> string list
  val to_field_list : t -> Field.t list
end

module type Schema = sig
  val deserialize : string -> string list
  val serialize : string list -> string
  val rep_ok : ?aux:(string -> string) -> string -> bool
  val search : string -> (string -> bool) -> string list option
  val add : string -> string -> (int, string) result
  val delete : string -> string list -> (int, string) result
  val update : string -> (string -> string) -> bool
end

type select_context = Sloppy | Strict

module type Cluster = sig
  module Entry : EntryType
  module Sch : Schema

  val filename : string ref
  val bind : string -> unit
  val unbind : unit -> unit
  val rep_ok : unit -> bool
  val search : select_context * (Field.t -> bool) -> Entry.t list
  val delete : select_context * (Field.t -> bool) -> (int, string) result
  val add : string list -> (int, string) result
  val update : Field.t -> select_context * (Field.t -> bool) -> bool
  val query : Query.t -> Entry.t list
  val change : Query.t -> (int, string) result
end

module type MakeCluster =
  functor (E : EntryType) ->
  functor (S : Schema) ->
    Cluster with module Entry = E and module Sch = S

(** TODO: replace with a regex checker *)
let string_contains str1 str2= 
  let len1 = String.length str1 and len2 = String.length str2 in
  if len1 < len2 then false else
    let rec check i = 
      if i > len1 - len2 then false else
      if (str2 = String.sub str1 i len2) then true else check (succ i) in
    check 0

let wrapup_ok ic oc temp filename =
  flush oc;
  close_in ic;
  close_out oc;
  Sys.rename temp filename

let wrapup_e ic oc temp =
  close_in ic;
  close_out oc;
  Sys.remove temp

(** [NumIDSchema] is the common file architecture for files
    containing numerical IDs. *)
module NumIDSchema : Schema = struct

  let deserialize line =
    String.split_on_char ';' line

  let serialize data =
    String.concat ";" data

  let search filename criterion = 
    let channel = open_in filename in
    let rec parse_line chnl acc= 
      try let x = input_line chnl in
        if criterion x then parse_line chnl (x :: acc) 
        else parse_line chnl acc
      with End_of_file -> acc in
    let results = parse_line channel [] in
    close_in channel;
    if List.length results != 0 then Some results 
    else None

  let get_id line =
    match String.index_opt line ';' with
    | None -> int_of_string line
    | Some delim -> int_of_string (String.sub line 0 delim)

  let total_lines filename = 
    let chnl = open_in filename in
    let top_line = input_line chnl in
    close_in chnl;
    match String.split_on_char ';' top_line with
    | [] -> 0
    | id::_ -> int_of_string id

  let rep_ok ?aux:(aux=fun x -> x) filename =
    let ic = open_in filename in
    let rec parse prev_id =
      try let curr = aux (input_line ic) in
        let curr_id = get_id curr in
        match prev_id with
        (* Just started parsing--keep going! *)
        | None -> parse (Some curr_id)
        (* Current id is 1 less than the previous *)
        | Some i -> if curr_id = pred i then parse (Some curr_id) else false
      with End_of_file -> close_in ic;
        match prev_id with
        (* File is empty--no complaints there! *)
        | None -> true
        (* Must end on one *)
        | Some i -> if i = 1 then true else false
        (* Failure catches two main exceptions: aux failure, and int_of_string
           failure (the line didn't start with an id). *)
    in try parse None with Failure e -> false

  let dec_id line amt =
    let delim = String.index line ';' in
    let id = int_of_string (String.sub line 0 delim) in
    let rest = String.sub line delim (String.length line - delim) in
    string_of_int (id - amt) ^ rest

  let add filename data = 
    let new_id = string_of_int (1 + total_lines filename) in
    let temp_file = filename ^ ".temp" in
    let ic = open_in filename in
    let oc = open_out temp_file in
    output_string oc (new_id ^ ";" ^ data);
    let rec add_line () = 
      try let line = input_line ic in
        output_char oc '\n'; output_string oc line;
        add_line ()
      with | End_of_file -> wrapup_ok ic oc temp_file filename; Ok 1
           | Sys_error e -> wrapup_e ic oc temp_file; Error e
    in add_line () 

  let delete filename selection =
    let length = total_lines filename in
    let num = List.length selection in
    let ic = open_in filename in
    let temp = filename ^ ".temp" in
    let oc = open_out temp in
    let rec process i selection amt =
      (* The top of selection will always be the one to delete next. *)
      try
        let line = input_line ic in
        let delete curr rem =
          if line <> curr then
            begin
              output_string oc (dec_id line amt);
              output_char oc '\n';
              process (pred i) (curr::rem) amt
            end
          else process (pred i) rem (pred amt) in
        match selection with
        | [] ->
          output_string oc line;
          output_char oc '\n';
          process (pred i) [] 0
        | curr::rem -> delete curr rem
      with | End_of_file -> wrapup_ok ic oc temp filename; Ok num
           | Sys_error e -> wrapup_e ic oc temp; Error e
    in process length selection num

  let update filename change =
    let ic = open_in filename in
    let temp = filename ^ ".temp" in
    let oc = open_out temp in
    let rec process i =
      try let line = input_line ic in
        output_string oc (change line);
        if i > 1 then output_char oc '\n';
        process (pred i)
      with | End_of_file -> wrapup_ok ic oc temp filename; true 
           | Sys_error e -> wrapup_e ic oc temp; false
    in process (total_lines filename)
end

module NoIDSchema : Schema = struct
  let deserialize line =
    String.split_on_char ';' line

  let serialize data =
    String.concat ";" data

  let rep_ok ?aux:(aux=Fun.id) filename =
    let ic = open_in filename in
    let rec parse () =
      try ignore (aux (input_line ic)); parse ()
      with | End_of_file -> close_in ic; true
           | Failure e -> false
    in parse ()

  let search filename criterion = 
    let channel = open_in filename in
    let rec parse_line chnl acc= 
      try let x = input_line chnl in
        if criterion x then parse_line chnl (x :: acc) 
        else parse_line chnl acc
      with End_of_file -> acc in
    let results = parse_line channel [] in
    close_in channel;
    if List.length results != 0 then Some results 
    else None

  let add filename data = 
    let temp_file = filename ^ ".temp" in
    let ic = open_in filename in
    let oc = open_out temp_file in
    output_string oc data;
    let rec add_line () = 
      try let line = input_line ic in
        output_char oc '\n'; output_string oc line;
        add_line ()
      with | End_of_file -> wrapup_ok ic oc temp_file filename; Ok 1 
           | Sys_error e -> wrapup_e ic oc temp_file; Error e
    in add_line () 

  let delete filename selection =
    let num = List.length selection in
    let ic = open_in filename in
    let temp = filename ^ ".temp" in
    let oc = open_out temp in
    let rec process selection amt =
      try
        let line = input_line ic in
        let delete curr rem =
          if line <> curr then
            begin
              output_string oc line;
              output_char oc '\n';
              process (curr::rem) amt
            end
          else process rem (pred amt) in
        match selection with
        | [] ->
          output_string oc line;
          output_char oc '\n';
          process [] 0
        | curr::rem -> delete curr rem
      with | End_of_file -> wrapup_ok ic oc temp filename; Ok num
           | Sys_error e -> wrapup_e ic oc temp; Error e
    in process selection num

  let update filename change =
    let ic = open_in filename in
    let temp = filename ^ ".temp" in
    let oc = open_out temp in
    let rec process () =
      try let line = input_line ic in
        output_string oc (change line);
        output_char oc '\n';
        process ()
      with | End_of_file -> wrapup_ok ic oc temp filename; true 
           | Sys_error e -> wrapup_e ic oc temp; false
    in process () 
end