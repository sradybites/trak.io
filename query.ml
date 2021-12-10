type t = 
    GET of from_clause * condition_clause
  | UPDATE of from_clause * replace_clause * condition_clause
  | DELETE of from_clause * condition_clause
  | ADD of into_clause * values_clause

and from_clause = FROM of string option

and into_clause = INTO of string option

and values_clause = VALUES of string list

and condition_clause = WHERE of expr

and replace_clause = REPLACE of Field.t

and expr = 
    NEQ of string * string 
  | EQ of string * string
  | IN of string * string list
  | AND of expr * expr 
  | OR of expr * expr

let ( ||| ) e1 e2 = OR (e1, e2)
let ( &&& ) e1 e2 = AND (e1, e2)
let ( >>> ) col l = IN (col, l)
let ( === ) s1 s2 = EQ (s1, s2)
let ( =/= ) s1 s2 = NEQ (s1, s2)