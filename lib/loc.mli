type t = {
  file : string;
  start_line : int;
  start_column : int;
  end_line : int;
  end_column : int;
}

val pretty : t -> string
