open Error
open Type

(*
 * syntax
 *)

type var = string
[@@deriving show]

type program = def list * body
and def = var * Type.t option * var list * body
and body =
  | SSkip
  | SSbst of var * Type.t * expr
  | SApp  of var * expr list
  | SSeq  of body * body
  | SIf   of expr * body * body
  | SNDet of body * body
and expr =
  | ENDBool
  | ENDInt
  (* the rest is almost as same as PiSyntax.expr *)
  | EVar  of var
  | EBool of bool
  | EInt  of int
  | ENot  of expr
  | EAnd  of expr * expr
  | EOr   of expr * expr
  | ENeg  of expr
  | EAdd  of expr * expr
  | ESub  of expr * expr
  | EMul  of expr * expr
  | EDiv  of expr * expr
  | EEq   of expr * expr
  | ELt   of expr * expr
  | EGt   of expr * expr
  | ELeq  of expr * expr
  | EGeq  of expr * expr
[@@deriving show]

(*
 * utility functions
 *)

type output_mode = C
let mode = ref C

module FuncsymEnv =
  Map.Make
    (struct
	type t = Type.t
	let compare = compare
      end)

type funcsymEnv = var FuncsymEnv.t
let funcsym_env = ref FuncsymEnv.empty
let _funcsym_i = ref 1

(* make_funcsym : Type.t -> var *)
let rec make_funcsym ty =
  match !mode with
  | C -> make_funcsymC ty
and make_funcsymC ty =
  try
    FuncsymEnv.find ty !funcsym_env
  with
  | Not_found ->
     (* TODO(nekketsuuu): Avoid dupulicating symbols *)
     let funcsym = "f_" ^ string_of_int !_funcsym_i in
     _funcsym_i := !_funcsym_i + 1;
     funcsym_env := FuncsymEnv.add ty funcsym !funcsym_env;
     funcsym

(*
 * pretty print
 *)

open Format

let tab_width = 4

let rec printC_program (defs, body) =
  open_vbox 0;
  print_string "int nondet();";
  print_cut ();
  print_cut ();
  print_string "typedef int bool;";
  print_cut ();
  print_string "bool true = 1;";
  print_cut ();
  print_string "bool false = 0;";
  print_cut ();
  print_string "int nondet_int() { return nondet(); }";
  print_cut ();
  print_string "bool nondet_bool() { return nondet() % 2; }";
  print_cut ();
  print_cut ();
  close_box ();
  printC_defs defs;
  open_vbox 0;
  open_vbox tab_width;
  print_string "int main(void) {";
  print_cut ();
  (if body <> SSkip then
     (printC_body [] body;
      print_cut ()));
  print_string "return 0;";
  close_box ();
  print_cut ();
  print_string "}";
  print_cut ();
  close_box ();
  print_flush ();
and printC_defs defs =
  open_vbox 0;
  List.iter
    (fun (f, tyo, xs, body) ->
     open_box tab_width;
     print_string @@ "void " ^ f ^ " (";
     (* TODO(nekketsuuu): 型をあわせる *)
     (match xs with
      | [] -> ()
      | x :: xs ->
	 print_string "int";
	 List.iter (fun _ -> print_string ", int") xs);
     print_string ");";
     close_box();
     print_space ())
    defs;
  print_space ();
  close_box ();
  List.iter printC_def defs
and printC_def (f, tyo, xs, body) =
  open_vbox 0;
  open_hbox ();
  (match tyo with
   | None -> ()
   | Some(ty) ->
      print_string "/*";
      print_space ();
      Type.print_t ty;
      print_space ();
      print_string "*/");
  close_box ();
  print_cut ();
  open_vbox tab_width;
  open_hbox ();
  print_string @@ "void " ^ f ^ "(";
  printC_vars xs;
  print_string ") {";
  close_box ();
  print_cut ();
  (if body <> SSkip then
     (open_box 0;
      printC_body xs body;
      close_box ();
      print_cut ()));
  print_string "return;";
  close_box ();
  print_cut ();
  print_string "}";
  print_cut ();
  print_cut ();
  close_box ();
and printC_vars xs =
  match xs with
  | [] -> ()
  | x :: xs ->
     print_string x;
     List.iter (fun x -> print_string ","; print_space (); print_string x) xs
(* Note: no newline after printing *)
and printC_body bounded_vars body =
  ignore @@ printC_body' bounded_vars body
(* printC_body' : var list -> body -> var list *)
and printC_body' bounded_vars body =
  match body with
  | SSkip -> bounded_vars
  | SSbst(x, ty, e) ->
     (* TODO(nekketsuuu): これで良いか確認する *)
     (* Note that var value can't be updated in pi *)
     let printC_decl () =
	 open_hbox ();
	 printC_type ty;
	 print_space ();
	 print_string @@ x ^ " = ";
	 printC_expr e;
	 print_string ";";
	 close_box ();
     in
     if List.mem x bounded_vars then
       begin
	 open_vbox 0;
	 open_vbox tab_width;
	 print_string "{";
	 print_cut ();
	 printC_decl ();
	 close_box ();
	 print_cut ();
	 print_string "}";
	 close_box ();
	 bounded_vars
       end
     else
       begin
	 printC_decl ();
	 add x bounded_vars
       end
  | SApp(f, es) ->
     print_string @@ f ^ "(";
     open_hovbox 0;
     printC_exprs es;
     close_box ();
     print_string ");";
     bounded_vars
  | SSeq(prog1, prog2) ->
     open_vbox 0;
     let bounded_vars = printC_body' bounded_vars prog1 in
     print_cut ();
     let bounded_vars = printC_body' bounded_vars prog2 in
     close_box ();
     bounded_vars
  | SIf(e, prog1, prog2) ->
     open_vbox 0;
     (* if clause *)
     open_vbox tab_width;
     open_hbox ();
     print_string "if ";
     print_string "(";
     printC_expr e;
     print_string ")";
     print_string " {";
     close_box ();
     print_cut ();
     printC_body bounded_vars prog1;
     close_box ();
     (* else clause *)
     print_cut ();
     open_vbox tab_width;
     print_string "} else {";
     print_cut ();
     printC_body bounded_vars prog2;
     close_box ();
     print_cut ();
     print_string "}";
     print_cut ();
     close_box ();
     bounded_vars
  | SNDet(prog1, prog2) ->
     printC_body' bounded_vars
		  (SIf(ENDBool, prog1, prog2))
and printC_exprs es =
  match es with
  | [] -> ()
  | e :: es ->
     open_hvbox 0;
     printC_expr e;
     List.iter (fun e -> print_string ","; print_space (); printC_expr e) es;
     close_box ();
and printC_expr e =
  match e with
  | ENDBool ->
     print_string "nondet_bool()"
  | ENDInt ->
     print_string "nondet_int()"
  | EVar(x) ->
     print_string x
  | EBool(b) ->
     print_bool b
  | EInt(i) ->
     print_int i
  | ENot(e) ->
     printC_unary_expr "!" e
  | ENeg(e) ->
     printC_unary_expr "-" e
  | EAnd(e1, e2) ->
     printC_binary_expr "&&" e1 e2
  | EOr(e1, e2) ->
     printC_binary_expr "||" e1 e2
  | EAdd(e1, e2) ->
     printC_binary_expr "+" e1 e2
  | ESub(e1, e2) ->
     printC_binary_expr "-" e1 e2
  | EMul(e1, e2) ->
     printC_binary_expr "*" e1 e2
  | EDiv(e1, e2) ->
     printC_binary_expr "/" e1 e2
  | EEq(e1, e2) ->
     printC_binary_expr "==" e1 e2
  | ELt(e1, e2) ->
     printC_binary_expr "<" e1 e2
  | EGt(e1, e2) ->
     printC_binary_expr ">" e1 e2
  | ELeq(e1, e2) ->
     printC_binary_expr "<=" e1 e2
  | EGeq(e1, e2) ->
     printC_binary_expr ">=" e1 e2
and printC_unary_expr op e =
  open_box @@ String.length op + 1;
  print_string @@ op ^ "(";
  printC_expr e;
  print_string ")";
  close_box ()
and printC_binary_expr op e1 e2 =
  (* TODO(nekketsuuu): もっと括弧まわりを綺麗に表示する *)
  open_box 1;
  print_string "(";
  printC_expr e1;
  print_space ();
  print_string op;
  print_space ();
  printC_expr e2;
  print_string ")";
  close_box ()
and printC_type ty =
  (* TODO(nekketsuuu): エラーメッセージ改善 *)
  match ty with
  | TBool ->
     print_string "bool"
  | TInt ->
     print_string "int"
  | TChan(_) ->
     raise @@ ConvErr("seqSyntax: Unable to print channel types")
  | TUnit ->
     raise @@ ConvErr("seqSyntax: Unable to print unit type")
  | TVar(_) ->
     raise @@ ConvErr("seqSyntax: Unable to print tvar types")
(* utility functions for bounded_vars *)
and add x bounded_vars =
  if List.mem x bounded_vars then bounded_vars
  else x :: bounded_vars
and union bvars1 bvars2 =
  match bvars2 with
  | [] -> bvars1
  | x :: xs -> union (add x bvars1) xs

let print_program prog =
  match !mode with
  | C -> printC_program prog

(*
 * debug
 *)

let prog = (
  [("f", ["x"], SSkip)],
  SSkip
)
