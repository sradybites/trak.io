(* You must [make build] before this file will work. 
   Then run [utop] and [#use] this file. 
   VS Code will complain about the # character below.
   That is expected behavior: this file is 
   meant to be used, not compiled.*)

#directory "_build";;
#load_rec "query.cmo";;

open Query;;

(* This is a quick signature to hide functions from the Cluster that
   aren't meant for use in interactive database mode. *)
module type QCluster = sig
  module Entry : Cluster.EntryType
  val filename : string ref
  val rep_ok : unit -> bool
  val query : t -> Entry.t list
  val change : t -> (int, string) result
end

(* The next six lines create Clusters for our three data types. *)
module Tasks : QCluster with module Entry = Types.Task = 
  MakeCluster.MakeCluster (Types.Task) (Cluster.NumIDSchema);;
module Teams : QCluster with module Entry = Types.Team = 
  MakeCluster.MakeCluster (Types.Team) (Cluster.NoIDSchema);;
module Users : QCluster with module Entry = Types.Login = 
  MakeCluster.MakeCluster (Types.Login) (Cluster.NoIDSchema);;