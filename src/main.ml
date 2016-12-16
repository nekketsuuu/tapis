open Syntax

let main ()  =
  try 
    let lexbuf = Lexing.from_channel stdin in 
    let result = Parser.toplevel Lexer.main lexbuf in
    print_endline @@ show_process result
  with
    | Parsing.Parse_error -> 
       print_endline @@ app_name ^ ": Parse_error"
    | LexErr(str)
    | ParseErr(str)
    | EvalErr(str)
    | TypeErr(str) ->
       print_endline @@ app_name ^ str

let _ = main ()
