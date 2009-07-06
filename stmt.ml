(** Statement *)

open Printf
open ExtString
open ListMore

(** optional name and start/end position in string *)
type param_id = string option * (int * int) deriving (Show) 
type param = param_id * Sql.Type.t deriving (Show)
type params = param list deriving (Show)

let params_to_string ps = Show.show<params>(ps)

type kind = | Select
            | Insert of RA.Schema.t option (** inserted *) * string (** table name *)
            | Create of string
            | Update of string
            | Delete of string
            | Alter of string
            | Drop of string
            deriving (Show)

type t = { schema : RA.Schema.t; params : params; kind : kind; props : Props.t; }

