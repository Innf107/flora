exception TODO of string

let todo loc = raise (TODO loc)

exception Panic of string * string

let panic loc message = raise (Panic (loc, message))

let rec map_cps : 'a 'b 'r. ('a -> ('b -> 'r) -> 'r) -> 'a list -> ('b list -> 'r) -> 'r =
  fun f list cont -> match list with
  | [] -> cont []
  | (x :: xs) -> 
    f x (fun y -> map_cps f xs (fun list -> cont (y :: list)))
(* TODO: Check that this is correct *)


let bind_cps : 'a 'r. (('a -> 'r) -> 'r) -> ('a -> 'r) -> 'r =
  fun f cont -> f cont