type t = {
  file : string;
  start_line : int;
  start_column : int;
  end_line : int;
  end_column : int;
}

let pretty t =
  t.file ^ ":" ^ string_of_int t.start_line ^ ":" ^ string_of_int t.start_column

let from_positions start_pos end_pos =
  Lexing.
    {
      file = start_pos.pos_fname;
      start_line = start_pos.pos_lnum;
      start_column = start_pos.pos_cnum - start_pos.pos_bol + 1;
      end_line = end_pos.pos_lnum;
      end_column = end_pos.pos_cnum - end_pos.pos_bol + 1;
    }
