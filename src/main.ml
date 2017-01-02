open Error
open Location
open Format

let app_name = "piterm"

let print_error str =
  set_formatter_out_channel stderr;
  print_endline @@ app_name ^ ": " ^ str;
  print_flush ()

(* verify : string -> string -> out_channel -> Lexing.from_channel -> unit *)
(* temp_outchan is closed in this function *)
let verify tool_cmd temp_filename temp_outchan lexbuf =
  try
    (* parse *)
    let process = Parser.toplevel Lexer.main lexbuf in
    (* type inference and program transformation *)
    let process = PiSyntax.closure process in
    Stype.infer process;
    let whole_program = Ttype.infer process in
    (* termination analysis of a sequential program *)
    set_formatter_out_channel temp_outchan;
    SeqSyntax.print_program whole_program;
    print_flush ();
    close_out temp_outchan;
    (* TODO(nekketsuuu): ここのtool_cmdはquoteしなくてよい？ *)
    let seqterm = tool_cmd ^ " " ^ (Filename.quote temp_filename) in
    let seqterm_exit_code = Sys.command seqterm in
    (* success? *)
    let is_term =
      if seqterm_exit_code = 0 then true
      else false
    in
    (* result *)
    set_formatter_out_channel stdout;
    open_vbox 0;
    begin
      open_vbox 4;
      print_string "input:";
      print_space ();
      open_box 0;
      PiSyntax.print_pl process;
      close_box ();
      close_box ();
      print_space ();
    end;
    begin
      open_vbox 4;
      print_string "tool:";
      print_space ();
      print_string seqterm;
      close_box ();
      print_space ();
    end;
    (* TODO(nekketsuuu): color *)
    begin
      open_vbox 4;
      print_string "RESULT:";
      print_space ();
      (if is_term then
	 print_string "SUCCESS! It's terminating."
       else
	 print_string "FAILED. I don't know its termination.");
      close_box ();
      print_space ();
    end;
    close_box ();
    print_flush ()
  (* TODO(nekketsuuu): exit with correct code *)
  with
  | Parsing.Parse_error ->
     print_error "Parse_error"
  | LexErr(str)
  | ParseErr(str)
  | EvalErr(str)
  | TypeErr(str)
  | ConvErr(str) ->
     print_error str

let rec interpret tool_cmd =
  print_string "> ";
  print_flush ();
  let lexbuf = Lexing.from_channel stdin in
  let (temp_filename, temp_outchan) = Filename.open_temp_file "temp" ".c" in
  begin
    try
      Sys.catch_break true;
      verify tool_cmd temp_filename temp_outchan lexbuf;
      close_out_noerr temp_outchan;
      Sys.remove temp_filename;
      Sys.catch_break false
    with
    | Sys.Break ->
       (close_out_noerr temp_outchan;
	Sys.remove temp_filename)
  end;
  interpret tool_cmd

let file tool_cmd filename =
  (* TODO(nekketsuuu): モードを増やすとき、拡張子に気をつける *)
  if Filename.extension filename = ".c" then
    print_error "The extension of input file must not be .c"
  else if Sys.file_exists filename then
    let inchan = open_in filename in
    let lexbuf = Lexing.from_channel inchan in
    let temp_filename =
      match !SeqSyntax.mode with
      | SeqSyntax.C -> (Filename.remove_extension filename) ^ ".c"
    in
    let temp_outchan = open_out temp_filename in
    verify tool_cmd temp_filename temp_outchan lexbuf;
    close_out_noerr temp_outchan
  else
    print_error @@ "No such file " ^ filename

(*
 * Main function
 *)

let _ =
  let filename = ref "" in
  let tool_cmd = ref "" in
  let is_interpret_mode = ref false in
  let arg_mode =
    Arg.String(fun s ->
	       if s = "C" then SeqSyntax.mode := SeqSyntax.C
	       else raise @@ Arg.Bad("Currently only C mode is supported.\n"))
  in
  let arg_tool = Arg.Set_string(tool_cmd) in
  let arg_interpret = Arg.Set(is_interpret_mode) in
  let speclist =
    [("-mode", arg_mode,
      "Choose a program transformation mode. Only C mode is supported currently. C is the default value");
     ("-tool", arg_tool,
      "Speicfy a path of the termination analysis tool such as TERMINATOR");
     ("-interpret", arg_interpret,
      "Run an interpreter");
     ("-m", arg_mode,
      "Same as -mode");
     ("-t", arg_tool,
      "Same as -tool");
     ("-i", arg_interpret,
      "Same as -interpret")]
  in
  let usage_msg =
    (app_name ^ " - Termination Analysis Tool of Pi-Calculus Processes\n" ^
       Printf.sprintf "usage: %s [-mode MODE] -tool TOOL -- FILE\n"
		      Sys.executable_name ^
	 Printf.sprintf "       %s -interpret" Sys.executable_name);
  in
  Arg.parse
    speclist
    (fun s -> if !filename = "" then filename := s)
    usage_msg;
  if !filename = "" then
    (print_endline "Error: No arguments";
     print_newline ();
     Arg.usage speclist usage_msg)
  else if !tool_cmd = "" then
    (print_endline "Error: The path of termination analysis tool for sequential programs is needed";
     print_newline ();
     Arg.usage speclist usage_msg)
  else if !is_interpret_mode then
    interpret !tool_cmd
  else
    file !tool_cmd !filename
