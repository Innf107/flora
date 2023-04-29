open Syntax

val serialize_env : out_channel -> env -> unit
val deserialize_env : in_channel -> env
