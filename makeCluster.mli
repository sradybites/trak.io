(** Implements creation of a module for a Database cluster storing tasks. *)

open Cluster

(** [MakeCluster] makes a [Cluster] that stores
    entries within a given file structure. *)
module MakeCluster : MakeCluster