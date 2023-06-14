exception TODO of string

let todo loc = raise (TODO loc)

exception Panic of string * string

let panic loc message = raise (Panic (loc, message))

let rec sequence_option = function
  | [] -> Some []
  | None :: _ -> None
  | Some value :: rest ->
      Option.map (fun r -> value :: r) (sequence_option rest)

let rec traverse_option f = function
  | [] -> Some []
  | value :: rest -> (
      match f value with
      | None -> None
      | Some value -> Option.map (fun r -> value :: r) (traverse_option f rest))

let compose list =
  List.fold_left (fun rest trans x -> trans (rest x)) Fun.id list

let compose_seq seq =
  Seq.fold_left (fun rest trans x -> trans (rest x)) Fun.id seq

let float_to_string f =
  if Float.is_integer f then
    Int.to_string (Float.to_int f)
  else
    Float.to_string f
