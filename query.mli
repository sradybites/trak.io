(**Implementaion of basic queries.*)

(** An acceptable query for the Cluster database. *)
type t = 
    GET of from_clause * condition_clause
  | UPDATE of from_clause * replace_clause * condition_clause
  | DELETE of from_clause * condition_clause
  | ADD of into_clause * values_clause

(** [FROM fn] is the file-binding flag to use. If fn is None,
    the Cluster will use its current binding. *)
and from_clause = FROM of string option

(** [INTO fn] is the file-binding flag to use in ADD queries. If fn is None,
    the Cluster will use its current binding.  *)
and into_clause = INTO of string option

(** [VALUES l] is the list of data [l] to append 
    to the Cluster in ADD queries. *)
and values_clause = VALUES of string list

(** [WHERE e] is the row predicate. Only rows that truthify [e] will
    be returned in a query using that row predicate. *)
and condition_clause = WHERE of expr

(** [REPLACE field] is the cell to be replaced in the database for a given
    query. If the field does not exist in the queried table, then nothing
    will be changed. *)
and replace_clause = REPLACE of Field.t

(** [expr] is a type of predicate expression that is translated by the Cluster
    to be applied to each row. *)
and expr = 
    NEQ of string * string
  | EQ of string * string
  | IN of string * string list
  | AND of expr * expr 
  | OR of expr * expr

(** [e1 ||| e2] is e1 or e2. *)
val ( ||| ) : expr -> expr -> expr

(** [e1 &&& e2] is e1 and e2. *)
val ( &&& ) : expr -> expr -> expr

(** [col >>> l] is a predicate expression asserting that the columnar value
    of label [col] is equal to a value in [l]. *)
val ( >>> ) : string -> string list -> expr

(** [col === val] is a predicate expression asserting that the columnar
    value of label [col] is equal to [val] *)
val ( === ) : string -> string -> expr

(** [col =/= val] is the opposite of [col === val]. *)
val ( =/= ) : string -> string -> expr