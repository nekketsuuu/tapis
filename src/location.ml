type 'a location = {
    loc_val:   'a;
    loc_start: Lexing.position; [@opaque]
    loc_end:   Lexing.position; [@opaque]
  }

let dummy_loc x = {
    loc_val   = x;
    loc_start = Lexing.dummy_pos;
    loc_end   = Lexing.dummy_pos;
  }

open Lexing

let show_pos2 ls le =
  if ls.pos_fname = le.pos_fname then
    Printf.sprintf
      "line %d, character %d -- line %d, character %d"
      ls.pos_lnum (ls.pos_cnum - ls.pos_bol)
      le.pos_lnum (le.pos_cnum - le.pos_bol)
  else
    Printf.sprintf
      "file \"%s\", line %d, character %d -- file \"%s\", line %d, character %d"
      ls.pos_fname ls.pos_lnum (ls.pos_cnum - ls.pos_bol)
      le.pos_fname le.pos_lnum (le.pos_cnum - le.pos_bol)

let show_loc { loc_start = ls; loc_end = le } =
  show_pos2 ls le
