%{
  open Syntax
  let parse_error s = (* Called by the parser function on error *)
    raise @@ ParseErr("Parser: Error")
%}

%token<int>    INT
%token<bool>   BOOL
%token<string> ID
%token NOT AND OR
%token PLUS MINUS DIV
%token EQ LT GT LEQ GEQ
%token IF THEN ELSE
%token LPAR RPAR
%token CAMMA
%token AST
%token ZERO QUESTION EXCLAMATION PERIOD PIPE NEW IN
%token EOF

%left NOT
%left AND OR
%left EQ LT GT LEQ GEQ
%left PLUS
%nonassoc MINUS
%left DIV
%nonassoc AST
%right PIPE

%start toplevel
%type <Syntax.process> toplevel
%%

toplevel:
    | process EOF { $1 }
;

process:
    | res_process          { $1 }
    | process PIPE process { PPar($1, $3) }
;

res_process:
    | if_process            { $1 }
    | NEW ID IN res_process { PRes($2, $4) }
;

if_process:
    | rin_process                             { $1 }
    | IF expr THEN if_process ELSE if_process { PIf($2, $4, $6) }
;

rin_process:
    | inout_process                            { $1 }
    | AST ID QUESTION chans PERIOD rin_process { PRIn($2, $4, $6) }
    | AST ID QUESTION chans                    { PRIn($2, $4, PNil) }
;

inout_process:
    | atomic_process                            { $1 }
    | ID QUESTION chans PERIOD inout_process    { PIn($1, $3, $5) }
    | ID EXCLAMATION exprs PERIOD inout_process { POut($1, $3, $5) }
    | ID QUESTION chans                         { PIn($1, $3, PNil) }
    | ID EXCLAMATION exprs                      { POut($1, $3, PNil) }
;

atomic_process:
    | LPAR process RPAR { $2 }
    | ZERO              { PNil }
;

chans:
    | LPAR chans_list RPAR { $2 }
    | ID                   { [$1] }
;

chans_list:
    | ID CAMMA chans_list { $1 :: $3 }
    | ID                  { [$1] }
;

exprs:
    | LPAR exprs_list RPAR { $2 }
    | expr                 { [$1] }
;

exprs_list:
    | expr CAMMA exprs_list_non_empty { $1 :: $3 }
;

exprs_list_non_empty:
    | expr CAMMA exprs_list { $1 :: $3 }
    | expr                  { [$1] }
;

expr:
    | comp_expr     { $1 }
    | NOT expr      { ENot($2) }
    | expr AND expr { EAnd($1, $3) }
    | expr OR expr  { EOr($1, $3) }
;

comp_expr:
    | mult_expr               { $1 }
    | comp_expr EQ comp_expr  { EEq($1, $3) }
    | comp_expr LT comp_expr  { ELt($1, $3) }
    | comp_expr GT comp_expr  { EGt($1, $3) }
    | comp_expr LEQ comp_expr { ELeq($1, $3) }
    | comp_expr GEQ comp_expr { EGeq($1, $3) }
;

mult_expr:
    | add_expr                { $1 }
    | add_expr AST mult_expr  { EMul($1, $3) }
    | mult_expr DIV mult_expr { EDiv($1, $3) }
;

add_expr:
    | atomic_expr                { $1 }
    | add_expr PLUS add_expr     { EAdd($1, $3) }
    | atomic_expr MINUS add_expr { ESub($1, $3) }
    | MINUS atomic_expr          { ENeg($2) }
;

atomic_expr:
    | LPAR expr RPAR { $2 }
    | LPAR RPAR      { EUnit }
    | BOOL           { EBool($1) }
    | INT            { EInt($1) }
    | ID             { EVar($1) }
;
