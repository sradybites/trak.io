open Cluster
open Query

module MakeCluster : MakeCluster = 
  functor (E : EntryType) -> 
  functor (S : Schema) -> struct

    module Entry = E
    module Sch = S
    type entry = Entry.t

    let filename = ref E.assoc_file

    let bind teamname = filename := (teamname ^ "_" ^ E.assoc_file)
    let unbind () = filename := E.assoc_file

    let verify line =
      Sch.deserialize line
      |> Entry.create_entry
      |> Entry.to_string_list
      |> Sch.serialize

    let rep_ok () = Sch.rep_ok ~aux:verify !filename

    let form_list (l : string list) : entry list =
      List.map Sch.deserialize l
      |> List.map Entry.create_entry

    let check (ctxt, criterion) entry =
      Entry.to_field_list entry
      |> match ctxt with
      | Sloppy -> List.exists criterion
      | Strict -> List.for_all criterion

    let select ctxt_criterion =
      let checker line =
        check ctxt_criterion (Entry.create_entry (Sch.deserialize line))
      in Sch.search !filename checker

    let search_query criteria =
      let check line = criteria (Entry.create_entry (Sch.deserialize line)) in
      match Sch.search !filename check with
      | None -> []
      | Some x -> form_list x

    let search ctxt_criterion =
      match select ctxt_criterion with
      | None -> []
      | Some x -> form_list x

    (* TODO: Check data is valid *)
    let add data = Sch.add !filename (Sch.serialize data)

    let delete_query criteria =
      let check line = criteria (Entry.create_entry (Sch.deserialize line)) in
      match Sch.search !filename check with
      | None -> Ok 0
      | Some l -> Sch.delete !filename (List.rev l)

    let delete ctxt_criterion =
      match select ctxt_criterion with
      | None -> Ok 0
      | Some l -> Sch.delete !filename (List.rev l)

    let update_query field criteria =
      let counter = ref 0 in
      let new_line upd line =
        let modify entry = 
          if criteria entry
          then (counter := !counter + 1; Entry.update_field upd entry)
          else entry
        in modify (Entry.create_entry (Sch.deserialize line))
           |> Entry.to_string_list
           |> Sch.serialize in
      if Sch.update !filename (new_line field) 
      then Ok !counter else Error "Critical Database Error"

    let update field ctxt_criterion =
      let new_line upd line =
        let modify entry = 
          if check ctxt_criterion entry 
          then Entry.update_field upd entry
          else entry
        in modify (Entry.create_entry (Sch.deserialize line))
           |> Entry.to_string_list
           |> Sch.serialize
      in Sch.update !filename (new_line field)

    let eval_from = function | None -> () | Some n -> bind n

    let rec query = function
      | GET (FROM fn, c) -> eval_from fn; search_query (eval_cond c)
      | UPDATE _ -> []
      | DELETE _ -> []
      | ADD _ -> []

    and change = function
      | GET _ -> Ok 0
      | UPDATE (FROM fn, REPLACE t, c) -> 
        eval_from fn; update_query t (eval_cond c)
      | DELETE (FROM fn, c) -> 
        eval_from fn; delete_query (eval_cond c)
      | ADD (INTO fn, VALUES data) ->
        eval_from fn; Sch.add !filename (Sch.serialize data)

    and eval_cond = function
      | WHERE e -> 
        fun entry -> List.exists (eval_expr e) (Entry.to_field_list entry)

    and eval_expr = function
      | NEQ (s1, s2) ->
        fun f -> not (Field.equal (Field.make_str_field s1 s2) f)
      | EQ (s1, s2) -> 
        fun f -> (Field.equal (Field.make_str_field s1 s2) f)
      | IN (col, l) -> fun f -> 
        List.exists (Field.equal f) (List.map (Field.make_str_field col) l)
      | AND (e1, e2) -> fun f -> eval_expr e1 f && eval_expr e2 f
      | OR (e1, e2) -> fun f -> eval_expr e1 f || eval_expr e2 f
  end