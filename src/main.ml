open Location
open Syntax

let rec main () =
  try 
    let lexbuf = Lexing.from_channel stdin in 
    let result = Parser.toplevel Lexer.main lexbuf in
    print_endline @@ show_process result.loc_val;
    main ()
  with
    | Parsing.Parse_error -> 
       (print_endline @@ app_name ^ ": Parse_error";
	main ())
    | LexErr(str)
    | ParseErr(str)
    | EvalErr(str)
    | TypeErr(str) ->
       (print_endline @@ app_name ^ ": " ^ str;
	main ())

let _ = main ()
