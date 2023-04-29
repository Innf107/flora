open Syntax

val deserialize_category : Trace.category

type deserialization_error =
  | EOF
  | InvalidTag of {
      ty : string;
      tag : int;
    }

exception DeserializationError of deserialization_error

val serialize_env : out_channel -> env -> unit
val deserialize_env : in_channel -> env
