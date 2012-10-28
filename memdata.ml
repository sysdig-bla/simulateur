
exception Not_found

module MemStorage = Map.Make(struct
	type t = string
        let compare = compare
end)

type t = (bool array) MemStorage.t

let empty = MemStorage.empty

let from_file filename =
	(* TODO *)
	MemStorage.empty

let get_data storage id =
	(* TODO *)
	[| |]
 
