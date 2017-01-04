%{
  open Error
  open Location
  open PiSyntax

  let annot_loc x = {
      loc_val   = x;
      loc_start = Parsing.symbol_start_pos ();
      loc_end   = Parsing.symbol_end_pos ();
    }
  let dummy_proc = Location.dummy_loc PNil

  let rec make_types ys =
    if ys = [] then []
    else None :: (make_types @@ List.tl ys)
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

/* (* lower to higher *) */
%left AND OR
%left EQ LT GT LEQ GEQ
%left PLUS MINUS
%left DIV
%nonassoc prec_unary_minus
%nonassoc prec_not
%nonassoc prec_new
%right PIPE
%right prec_if
%nonassoc prec_inout

%start toplevel
%type <PiSyntax.pl> toplevel
%%

toplevel:
    | process EOF
	{ $1 }
    | error
	{ raise @@
	    ParseErr(Printf.sprintf
		       "Parser: Syntex error near %s"
		       (show_pos2
			  (Parsing.symbol_start_pos ())
			  (Parsing.symbol_end_pos ()))) }
;

process:
    | atomic_process
        { $1 }
    | process PIPE process
	{ annot_loc @@ PPar({ pl1 = $1; pl2 = $3 }) }
    | NEW ID IN process
      %prec prec_new
	{ annot_loc @@ PRes({ x    = $2;
	                      tyxo = None;
	                      pl   = $4;   }) }
    | IF expr THEN process ELSE process
      %prec prec_if
	{ annot_loc @@ PIf({ el = $2; pl1 = $4; pl2 = $6 }) }
    | AST ID QUESTION chans PERIOD process
      %prec prec_inout
	{ annot_loc @@ PRIn({ x     = $2;
	                      ys    = $4;
	                      pl    = $6;            }) }
    | AST ID QUESTION chans
	{ annot_loc @@ PRIn({ x     = $2;
	                      ys    = $4;
	                      pl    = dummy_proc;    }) }
    | ID QUESTION chans PERIOD process
      %prec prec_inout
	{ annot_loc @@ PIn({ x     = $1;
	                     ys    = $3;
	                     pl    = $5;            }) }
    | ID EXCLAMATION exprs PERIOD process
      %prec prec_inout
	{ annot_loc @@ POut({ x    = $1;
	                      els  = $3;
	                      pl   = $5;   }) }
    | ID QUESTION chans
	{ annot_loc @@ PIn({ x     = $1;
	                     ys    = $3;
	                     pl    = dummy_proc;    }) }
    | ID EXCLAMATION exprs
	{ annot_loc @@ POut({ x    = $1;
	                      els  = $3;
	                      pl   = dummy_proc; }) }
;

atomic_process:
    | LPAR process RPAR
	{ $2 }
    | ZERO
	{ annot_loc @@ PNil }
;

chans:
    | LPAR chans_list RPAR
	{ $2 }
    | ID
	{ [$1] }
;

chans_list:
    | ID CAMMA chans_list
	{ $1 :: $3 }
    | ID
	{ [$1] }
;

exprs:
    | LPAR exprs_list RPAR
	{ $2 }
    | expr
	{ [$1] }
;

/* (* Be careful about the atomic expr `LPAR expr RPAR' *) */
exprs_list:
    | expr CAMMA exprs_list_length_gt_1
	{ $1 :: $3 }
;

exprs_list_length_gt_1:
    | expr CAMMA exprs_list_length_gt_1
	{ $1 :: $3 }
    | expr
	{ [$1] }
;

expr:
    | non_mul_expr
	{ $1 }
    | non_mul_expr AST expr
	{ annot_loc @@ EMul($1, $3) }

non_mul_expr:
    | atomic_expr
	{ $1 }
    | NOT non_mul_expr
      %prec prec_not
	{ annot_loc @@ ENot($2) }
    | MINUS non_mul_expr
      %prec prec_unary_minus
	{ annot_loc @@ ENeg($2) }
    | non_mul_expr AND non_mul_expr
	{ annot_loc @@ EAnd($1, $3) }
    | non_mul_expr OR non_mul_expr
	{ annot_loc @@ EOr($1, $3) }
    | non_mul_expr PLUS non_mul_expr
	{ annot_loc @@ EAdd($1, $3) }
    | non_mul_expr MINUS non_mul_expr
	{ annot_loc @@ ESub($1, $3) }
    | non_mul_expr DIV non_mul_expr
	{ annot_loc @@ EDiv($1, $3) }
    | non_mul_expr EQ non_mul_expr
	{ annot_loc @@ EEq($1, $3) }
    | non_mul_expr LT non_mul_expr
	{ annot_loc @@ ELt($1, $3) }
    | non_mul_expr GT non_mul_expr
	{ annot_loc @@ EGt($1, $3) }
    | non_mul_expr LEQ non_mul_expr
	{ annot_loc @@ ELeq($1, $3) }
    | non_mul_expr GEQ non_mul_expr
	{ annot_loc @@ EGeq($1, $3) }
;

atomic_expr:
    | LPAR expr RPAR
        { $2 }
    | LPAR RPAR
        { annot_loc @@ EUnit }
    | BOOL
        { annot_loc @@ EBool($1) }
    | INT
        { annot_loc @@ EInt($1) }
    | ID
        { annot_loc @@ EVar($1) }
;
