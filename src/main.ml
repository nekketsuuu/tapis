open Error
open Location
open PiSyntax

let app_name = "piterm"

let rec main () =
  try
    print_string "> ";
    flush stdout;
    let lexbuf = Lexing.from_channel stdin in
    let process = Parser.toplevel Lexer.main lexbuf in
    let process = PiSyntax.closure process in
    Stype.infer process;
    PiSyntax.print_pl process;
    main ()
  with
  | Parsing.Parse_error ->
     (print_endline @@ app_name ^ ": Parse_error";
      flush stdout;
      main ())
  | LexErr(str)
  | ParseErr(str)
  | EvalErr(str)
  | TypeErr(str) ->
     (print_endline @@ app_name ^ ": " ^ str;
      flush stdout;
      main ())

let _ = main ()
