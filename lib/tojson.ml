type error = Unsupported of [ `Closure | `Continuation ]

exception Error of error

type target = out_channel

type obj = {
  out_channel : out_channel;
  mutable written : bool;
}

type writer = target -> unit

let output_to out_channel writer = writer out_channel

let obj cont target =
  output_string target "{";
  cont { out_channel = target; written = false };
  output_string target "}"

let entry obj key writer =
  if obj.written then begin
    output_string obj.out_channel ", "
  end;
  obj.written <- true;

  output_string obj.out_channel "\"";
  output_string obj.out_channel key;
  output_string obj.out_channel "\": ";
  writer obj.out_channel

let null target = output_string target "null"
let bool bool target = output_string target (string_of_bool bool)

let string str target =
  output_string target "\"";
  (* TODO: Escape the string *)
  output_string target str;
  output_string target "\""

let number float target = output_string target (Float.to_string float)

let list writer_from_item list target =
  output_string target "[";
  begin
    match list with
    | [] -> ()
    | item :: rest ->
        writer_from_item item target;
        List.iter
          (fun item ->
            output_string target ", ";
            writer_from_item item target)
          rest
  end;
  output_string target "]"

let rec value = function
  | Syntax.Nil -> null
  | Number x -> number x
  | String x -> string x
  | Bool x -> bool x
  | List items -> list value items
  | Closure _ -> raise (Error (Unsupported `Closure))
  | Primop _ -> raise (Error (Unsupported `Closure))
  | Continuation _ -> raise (Error (Unsupported `Continuation))
