(**Implementation of Functors to create a module responsible for all data 
   operations on the database.*)

(** A module that matches [EntryType] is suitable for use as the type of entry
    in a [Cluster]. *)
module type EntryType = sig
  (**Type representing the data type.*)
  type t

  (**File associated with that particular data type.*)
  val assoc_file : string

  (**[create_entry str] is the entry made of str in type t.*)
  val create_entry : string list -> t

  (**[update_field field value] is the updated value in field of value value.
     Updates the assoc_file too.*)
  val update_field : Field.t -> t -> t

  (**[to_string_list v] is the list formed from type t value v.*)
  val to_string_list : t -> string list

  (**[to_field_list v] is the field list formed from type t value v.*)
  val to_field_list : t -> Field.t list
end

(* Schema will probably be the same for all of our files, but should allow us
   to mix things up a bit if we want! *)
(** A [Schema] provides information and functions common to a given
    file architecture. *)
module type Schema = sig

  (** [deserialize line] is the deserialized version of [line]. A Schema
      may have a unique way of denoting a new cell, for example a comma
      or a semicolon, that this function removes.
      [serialize (deserialize line) = line] *)
  val deserialize : string -> string list

  (** [serialize data] is the serialized version of [data]. *)
  val serialize : string list -> string

  (** [rep_ok ?aux filename] is whether the file corresponding to [filename]
      adheres to the specified Schema. Since Schemas are used in conjunction
      with other modules, an [aux] function may be provided to check further
      line-specific conditions.
      [aux line] is [line] if the line is valid, and raises [Failure] if
      otherwise. If no function is provided, [aux line] is [id line]. *)
  val rep_ok : ?aux:(string -> string) -> string -> bool

  (** [search filename criteria] is the result of a search on [filename] with 
      entries, if any, satisfying the [criteria] function.
      Requires: [filename] adheres to the specified [Schema]. *)
  val search : string -> (string -> bool) -> string list option

  (** [add filename line] is [Ok 1] if [line] was successfully added to
      [filename], and [Error e] otherwise.
      Requires: [filename] adheres to the specified [Schema]. *)
  val add : string -> string -> (int, string) result

  (** [delete filename selection] is [Ok List.length selection] if the operation
      is successful, and [Error e] otherwise.
      Requires: [filename] adheres to the specified [Schema]. *)
  val delete : string -> string list -> (int, string) result

  (** [update filename change] is true if line(s) were altered according to 
      [change], and false otherwise.
      Requires: [filename] adheres to the specified [Schema]. *)
  val update : string -> (string -> string) -> bool
end

(** A [select_context] tells the Cluster how it should apply a selection
      criterion when searching, deleting, or updating. There are two modes:
      sloppy and strict. Both check the criterion against each column,
      except sloppy mode will select an entry if at least one column matches
      the criterion, whereas in strict mode, every column must match. *)
type select_context = Sloppy | Strict

(** A [Cluster] stores data entries in a plaintext file as part of a
    decentralized database. *)
module type Cluster = sig

  (** [Entry] is a module representing the type of
      entry in the cluster and functions on them. *)
  module Entry : EntryType

  (** [Sch] is a module with information and functions on the file structure
      important to database operations. *)
  module Sch : Schema

  (** [filename] is the name of the plaintext file where data is stored, and
      the current domain of all functions. *)
  val filename : string ref

  (** [bind teamname] focuses the cluster on the file for [teamname]. If the
      cluster is already bound, it is rebound. 
      File-binding is a useful organizational technique that may or may not be
      employed in the application. Different files may exist for a given
      cluster, separated nominally by some unique id (in our case, we may
      make use of team name to sort tasks.) *)
  val bind : string -> unit

  (** [unbind ()] unbinds the cluster from the current team-specific file. *)
  val unbind : unit -> unit

  (** [rep_ok ()] is whether the current bound file is a valid Cluster according
      to the Sch and Entry implementations. *)
  val rep_ok : unit -> bool

  (** [search (ctxt, criterion)] is a list containing all entries that match the
      criterion, if any. [ctxt] determines how the criterion is applied. *)
  val search : select_context * (Field.t -> bool) -> Entry.t list

  (** [delete (ctxt, criterion)] is [Ok x] if [x] entries meeting [criterion] 
      were deleted, and [Error e] if an exception described in [e] was raised
      in the process, leaving the cluster untouched.
      [ctxt] determines how the criterion is applied. *)
  val delete : select_context * (Field.t -> bool) -> (int, string) result

  (** [add data] writes the given data to an entry in the cluster. Data is
      presented as a ordered list of fields corresponding to the entry type.
      Returns [Ok x] if [x] entries were added, and [Error e] if an exception
      with message [e] was raised in the process, leaving the cluster
      unmodified. *)
  val add : string list -> (int, string) result

  (** [update change (ctxt * criterion)] edits the entry(s) matching [criterion]
      with [change]. [ctxt] determines how the criterion is applied.*)
  val update : Field.t -> select_context * (Field.t -> bool) -> bool

  (** [query q] is the list of entries that qualify the query [q].
      Only GET queries may cause this function to return a non-empty list. *)
  val query : Query.t -> Entry.t list

  (** [change q] is [Ok x] if [x] entries were modified by the database
      operation, and [Error e] if an exception was thrown with 
      error message [e]. Primarily operates on query statements that cause
      data modification; GET queries will always be [Ok 0]. *)
  val change : Query.t -> (int, string) result
end

(** [MakeCluster] is a functor that makes a [Cluster] out of
    a module representing the data entry type, and a module with information
    on file architecture. *)
module type MakeCluster =
  functor (E : EntryType) ->
  functor (S : Schema) ->
    Cluster with module Entry = E and module Sch = S

(** [NumIDSchema] is the common file architecture for files
    containing numerical IDs. *)
module NumIDSchema : Schema

(** [NoIDSchema] is the file archictecture for files without numerical IDs. *)
module NoIDSchema : Schema