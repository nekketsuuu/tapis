{
  open Error
  open Location
  open PiSyntax
}

let digit = ['0'-'9']
let space = ' ' | '\t' | '\r' | '\n'
let alpha = ['a'-'z' 'A'-'Z' '_' ]
let ident = alpha (alpha | digit)*

rule main = parse
| space+       { main lexbuf }
| '\n'         { Lexing.new_line lexbuf;
		 main lexbuf }
| "/*"         { comment lexbuf;
		 main lexbuf }
| "O"          { Parser.ZERO }
| "?"          { Parser.QUESTION }
| "!"          { Parser.EXCLAMATION }
| "."          { Parser.PERIOD }
| "*"          { Parser.AST }
| "|"          { Parser.PIPE }
| "new"        { Parser.NEW }
| "in"         { Parser.IN }
| "if"         { Parser.IF }
| "then"       { Parser.THEN }
| "else"       { Parser.ELSE }
| "("          { Parser.LPAR }
| ")"          { Parser.RPAR }
| ","          { Parser.CAMMA }
| "not"        { Parser.NOT }
| "&&"         { Parser.AND }
| "||"         { Parser.OR }
| "+"          { Parser.PLUS }
| "-"          { Parser.MINUS }
| "/"          { Parser.DIV }
| "="          { Parser.EQ }
| "<"          { Parser.LT }
| ">"          { Parser.GT }
| "<="         { Parser.LEQ }
| ">="         { Parser.GEQ }
| "true"       { Parser.BOOL(true) }
| "false"      { Parser.BOOL(false) }
| eof          { Parser.EOF }
| digit+ as n  { Parser.INT(int_of_string n) }
| ident+ as id { Parser.ID(id) }
| _            { raise @@
		   LexErr(Printf.sprintf
			    "Lexer: Unknown token \"%s\" near %s"
			    (Lexing.lexeme lexbuf)
			    (show_pos2
			       (Lexing.lexeme_start_p lexbuf)
			       (Lexing.lexeme_end_p lexbuf))) }

and comment = parse
| "*/" { () }
| "/*" { comment lexbuf;
	 comment lexbuf }
| eof  { raise @@ LexErr("Lexer: Unterminated comment") }
| '\n' { Lexing.new_line lexbuf;
	 comment lexbuf }
| _    { comment lexbuf }
