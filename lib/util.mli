exception TODO of string

(* Should be used as `todo __LOC__` *)
val todo : string -> 'a

exception Panic of string * string

(* Should be used as `panic __LOC__ message`*)
val panic : string -> string -> 'a

val map_cps : ('a -> ('b -> 'r) -> 'r) -> 'a list -> ('b list -> 'r) -> 'r

val bind_cps : (('a -> 'r) -> 'r) -> ('a -> 'r) -> 'r