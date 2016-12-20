open Location
open Syntax

let rec loop () =
  main ();
  loop ()
and main () =
  try
    let process = parse () in
    let process = Stype.infer process in
    print_endline @@ show_process process.loc_val
  with
  | Parsing.Parse_error ->
     print_endline @@ app_name ^ ": Parse_error";
     flush stdout
  | LexErr(str)
  | ParseErr(str)
  | EvalErr(str)
  | TypeErr(str) ->
     print_endline @@ app_name ^ ": " ^ str;
     flush stdout
and parse () =
  print_string "> ";
  flush stdout;
  let lexbuf = Lexing.from_channel stdin in
  Parser.toplevel Lexer.main lexbuf
