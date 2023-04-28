type t = {
  file : string;
  start_line : int;
  start_column : int;
  end_line : int;
  end_column : int;
}

let pretty t =
  t.file ^ ":" ^ string_of_int t.start_line ^ ":" ^ string_of_int t.start_column
