open Syntax

val deserialize_category : Trace.category

type deserialization_error =
  | EOF
  | InvalidTag of {
      ty : string;
      tag : int;
    }
  | ContTypeError of {
      expected : string;
      actual : string;
    }

exception DeserializationError of deserialization_error

type serialization_target =
  | SerializeEnv of env
  | SerializeCont : ('a, 'r) Eval.cont -> serialization_target

val serialize : out_channel -> serialization_target -> unit

type 'r cont_result =
  | Value : value cont_result
  | EnvValue : (env * value) cont_result

type 'a deserialization_target =
  | DeserializeEnv : env deserialization_target
  | DeserializeCont : 'r cont_result -> (value, 'r) Eval.cont deserialization_target

val deserialize : 'a deserialization_target -> in_channel -> 'a
