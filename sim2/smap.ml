(*  *)

let k = 63 (* Alphabet size *)

type 'a node = { x:'a option ; next:'a t array ; loc:string }

and 'a t = 'a node option

let ioc c = match int_of_char c with
  | 95 -> 36
  | n when 47 < n && n < 58 -> n-48
  | n when 64 < n && n < 91 -> n-55
  | n when 96 < n && n < 123 -> n-60
  | _ -> invalid_arg ("char must be _0-9A-Za-z :"^String.make 1 c)

let empty = None

let find s t =
  let n = String.length s in
  let rec find i = function
    | Some t -> if i = n
          then t.x
          else find (i+1) t.next.(ioc s.[i])
    | None -> raise Not_found
  in match find 0 t with
    | Some x -> x
    | None -> raise Not_found

let mem s t = try ignore (find s t); true with Not_found -> false

let add s x t =
  let n = String.length s in
  let rec add i = function
    | Some t ->
        if i = n
          then Some { t with x=Some x ; loc=s }
          else begin
            let nx = Array.copy t.next in
            let c = ioc s.[i] in
            nx.(c) <- add (i+1) nx.(c);
            Some { t with next=nx }
          end
    | None ->
        let t = Array.make k None in
        if i = n
          then begin
            Some { x=Some x ; next=t ; loc=s}
          end
          else begin
            t.(ioc s.[i]) <- add (i+1) empty;
            Some { x=None ; next=t ; loc=""}
          end
  in add 0 t

let singleton s x = add s x empty

let fold f t y =
  let rec fold y = function
    | None -> y
    | Some t ->
        let y = match t.x with
          | None -> y
          | Some x -> f t.loc x y
        in Array.fold_left fold y t.next
  in
  fold y t

let rec iter f = function
  | None -> ()
  | Some t ->
    begin
      match t.x with
        | None -> ()
        | Some x -> f x
    end;
    Array.iter (iter f) t.next
