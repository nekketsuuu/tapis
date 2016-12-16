%{
  open Location
  open Syntax

  let annot_loc x = {
      loc_val   = x;
      loc_start = Parsing.symbol_start_pos ();
      loc_end   = Parsing.symbol_end_pos ();
    }
  let dummy_proc = Location.loc_dummy PNil

  let annot_type id = (id, None)
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
%type <Syntax.t> toplevel
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
    | res_process     
	{ $1 }
    | process PIPE process
	{ annot_loc @@ PPar($1, $3) }
;

res_process:
    | if_process            
	{ $1 }
    | NEW ID IN res_process
	{ annot_loc @@ PRes(annot_type $2, $4) }
;

if_process:
    | rin_process                             
	{ $1 }
    | IF expr THEN if_process ELSE if_process 
	{ annot_loc @@ PIf($2, $4, $6) }
;

rin_process:
    | inout_process                            
	{ $1 }
    | AST ID QUESTION chans PERIOD rin_process 
	{ annot_loc @@ PRIn(annot_type $2, $4, $6) }
    | AST ID QUESTION chans                    
	{ annot_loc @@ PRIn(annot_type $2, $4, dummy_proc) }
;

inout_process:
    | atomic_process                            
	{ $1 }
    | ID QUESTION chans PERIOD inout_process    
	{ annot_loc @@ PIn(annot_type $1, $3, $5) }
    | ID EXCLAMATION exprs PERIOD inout_process 
	{ annot_loc @@ POut(annot_type $1, $3, $5) }
    | ID QUESTION chans                        
	{ annot_loc @@ PIn(annot_type $1, $3, dummy_proc) }
    | ID EXCLAMATION exprs                      
	{ annot_loc @@ POut(annot_type $1, $3, dummy_proc) }
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
	{ [annot_type $1] }
;

chans_list:
    | ID CAMMA chans_list 
	{ annot_type $1 :: $3 }
    | ID                  
	{ [annot_type $1] }
;

exprs:
    | LPAR exprs_list RPAR 
	{ $2 }
    | expr                 
	{ [$1] }
;

exprs_list:
    | expr CAMMA exprs_list_non_empty 
	{ $1 :: $3 }
;

exprs_list_non_empty:
    | expr CAMMA exprs_list
	{ $1 :: $3 }
    | expr                 
	{ [$1] }
;

expr:
    | comp_expr     
	{ $1 }
    | NOT expr      
	{ annot_loc @@ ENot($2) }
    | expr AND expr 
	{ annot_loc @@ EAnd($1, $3) }
    | expr OR expr  
	{ annot_loc @@ EOr($1, $3) }
;

comp_expr:
    | mult_expr               
        { $1 }
    | comp_expr EQ comp_expr  
        { annot_loc @@ EEq($1, $3) }
    | comp_expr LT comp_expr  
        { annot_loc @@ ELt($1, $3) }
    | comp_expr GT comp_expr  
        { annot_loc @@ EGt($1, $3) }
    | comp_expr LEQ comp_expr 
        { annot_loc @@ ELeq($1, $3) }
    | comp_expr GEQ comp_expr 
        { annot_loc @@ EGeq($1, $3) }
;

mult_expr:
    | add_expr                
        { $1 }
    | add_expr AST mult_expr  
        { annot_loc @@ EMul($1, $3) }
    | mult_expr DIV mult_expr 
        { annot_loc @@ EDiv($1, $3) }
;

add_expr:
    | atomic_expr                
        { $1 }
    | add_expr PLUS add_expr     
        { annot_loc @@ EAdd($1, $3) }
    | atomic_expr MINUS add_expr 
        { annot_loc @@ ESub($1, $3) }
    | MINUS atomic_expr          
        { annot_loc @@ ENeg($2) }
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
        { annot_loc @@ EVar(annot_type $1) }
;
