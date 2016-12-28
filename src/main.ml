open Error
open Location

let app_name = "piterm"

let rec main () =
  try
    print_string "> ";
    flush stdout;
    let lexbuf = Lexing.from_channel stdin in
    let process = Parser.toplevel Lexer.main lexbuf in
    let process = PiSyntax.closure process in
    Stype.infer process;
    let whole_program = Ttype.infer process in
    PiSyntax.print_pl process;
    print_newline ();
    SeqSyntax.print_program whole_program;
    main ()
  with
  | Parsing.Parse_error ->
     (print_endline @@ app_name ^ ": Parse_error";
      flush stdout;
      main ())
  | LexErr(str)
  | ParseErr(str)
  | EvalErr(str)
  | TypeErr(str)
  | ConvErr(str) ->
     (print_endline @@ app_name ^ ": " ^ str;
      flush stdout;
      main ())

let _ = main ()
