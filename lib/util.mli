exception TODO of string

(* Should be used as `todo __LOC__` *)
val todo : string -> 'a

exception Panic of string * string

(* Should be used as `panic __LOC__ message`*)
val panic : string -> string -> 'a

val sequence_option : 'a option list -> 'a list option
val traverse_option : ('a -> 'b option) -> 'a list -> 'b list option

val compose : ('a -> 'a) list -> 'a -> 'a
val compose_seq : ('a -> 'a) Seq.t -> 'a -> 'a

val float_to_string : float -> string
