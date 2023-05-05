type error =
  | Unsupported of [`Closure | `Continuation]

exception Error of error

type target

type obj

type writer

val output_to : out_channel -> writer -> unit

val obj : (obj -> unit) -> writer

val entry : obj -> string -> writer -> unit

val null : writer

val bool : bool -> writer

val string : string -> writer

val number : float -> writer

val list : ('a -> writer) -> 'a list -> writer

val value : Syntax.value -> writer
