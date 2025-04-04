type  t = (string, string * Sql.select_complete) Hashtbl.t

let shared_queries: t = Hashtbl.create 15

let add = Hashtbl.add shared_queries

let get name = Hashtbl.find shared_queries name

let mem name = Hashtbl.mem shared_queries name
