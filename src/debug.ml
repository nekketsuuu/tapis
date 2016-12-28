open Error
open Location
open PiSyntax

let app_name = "piterm-debug"

let rec loop () =
  main ();
  loop ()
and main () =
  try
    let process = PiSyntax.closure @@ parse () in
    Stype.infer process;
    let whole_program = Ttype.infer process in
    PiSyntax.print_pl process;
    print_newline ();
    SeqSyntax.print_program whole_program
  with
  | Parsing.Parse_error ->
     print_endline @@ app_name ^ ": Parse_error";
     flush stdout
  | LexErr(str)
  | ParseErr(str)
  | EvalErr(str)
  | TypeErr(str)
  | ConvErr(str) ->
     print_endline @@ app_name ^ ": " ^ str;
     flush stdout
and parse () =
  print_string "> ";
  flush stdout;
  let lexbuf = Lexing.from_channel stdin in
  Parser.toplevel Lexer.main lexbuf
