open Location

exception LexErr of string
exception ParseErr of string
exception EvalErr of string
exception TypeErr of string

(*
 * syntax
 *)

type name = string
[@@deriving show]

(*
 * In order to make type annotations mutable,
 * I use a record for the body of process.
 * And because the processing of PIn and PRIn
 * are sometimes the same, I define "pin_body".
 * However, we cannot simply make "pout_body"
 * since there are same name labels.
 * Although we can construct that by using
 * recursive module etc., I avoid making types
 * too complicated.
 *)
(* TODO(nekketsuuu): closedなので型アノテーションはPResにだけあればいいのでは *)
type t = process location
and process =
  | PNil
  | PIn   of pin_body
  | PRIn  of pin_body
  | POut  of { x: name;
	       mutable tyxo: Type.t option;
	       els: e list;
	       pl: t; }
  | PPar  of { pl1: t; pl2: t }
  | PRes  of { x: name;
	       mutable tyxo: Type.t option;
	       pl: t; }
  | PIf   of { el: e; pl1: t; pl2: t }
and pin_body =
  { x:  name;
    ys: name list;
    mutable tyxo:  Type.t option;
    mutable tyyos: Type.t option list;
    pl: t; }
and e = expr location
and expr =
  | EVar  of name
  | EUnit
  | EBool of bool
  | EInt  of int
  | ENot  of e
  | EAnd  of e * e
  | EOr   of e * e
  | ENeg  of e
  | EAdd  of e * e
  | ESub  of e * e
  | EMul  of e * e
  | EDiv  of e * e
  | EEq   of e * e
  | ELt   of e * e
  | EGt   of e * e
  | ELeq  of e * e
  | EGeq  of e * e
[@@deriving show]

(*
 * utility functions
 *)

(* free_name : t -> name list *)
let rec free_name pl = free_name_b [] pl
and free_name_b bounded_names pl =
  match pl.loc_val with
  | PNil -> []
  | PIn(body)
  | PRIn(body) ->
     let bounded_names' = union body.ys bounded_names in
     let free_names = free_name_b bounded_names' body.pl in
     if List.mem body.x bounded_names then
       free_names
     else
       add body.x free_names
  | POut(body) ->
     let names = List.fold_left
		   (fun names el -> union names (name_expr el))
		   []
		   body.els in
     let free_names = diff names bounded_names in
     let free_names' = free_name_b bounded_names body.pl in
     let free_names = union free_names free_names' in
     if List.mem body.x bounded_names then
       free_names
     else
       add body.x free_names
  | PPar(body) ->
     union (free_name_b bounded_names body.pl1)
	   (free_name_b bounded_names body.pl2)
  | PRes(body) ->
     free_name_b (add body.x bounded_names) body.pl
  | PIf(body) ->
     let names = name_expr body.el in
     let free_names = diff names bounded_names in
     let free_names' = union (free_name_b bounded_names body.pl1)
			     (free_name_b bounded_names body.pl2) in
     union free_names free_names'
and name_expr el =
  match el.loc_val with
  | EVar(x) -> [x]
  | EUnit | EBool(_) | EInt(_) -> []
  | ENot(e)
  | ENeg(e) ->
     name_expr e
  | EAnd(e1, e2) | EOr(e1, e2)
  | EAdd(e1, e2) | ESub(e1, e2) | EMul(e1, e2) | EDiv(e1, e2)
  | EEq(e1, e2) | ELt(e1, e2) | EGt(e1, e2) | ELeq(e1, e2) | EGeq(e1, e2) ->
     union (name_expr e1) (name_expr e2)
(* utility functions for using a list as a set *)
and remove x names = List.filter (fun name -> name != x) names
and add x names =
  if List.mem x names then names
  else x :: names
and union names1 names2 =
  if names2 = [] then names1
  else
    let name = List.hd names2 in
    let names2' = List.tl names2 in
    if List.mem name names1 then
      union names1 names2'
    else
      name :: (union names1 names2')
and diff names1 names2 =
  if names2 = [] then names1
  else
    let name = List.hd names2 in
    let names2' = List.tl names2 in
    diff (remove name names1) names2'

(* closure : t -> name list *)
let closure pl =
  let free_names = free_name pl in
  let restrict pl name =
    (Printf.eprintf "Warning: A free name %s is restricted globally\n%!" name;
     Location.dummy_loc @@ PRes({ x = name; tyxo = None; pl = pl })) in
  List.fold_left restrict pl free_names

(*
 * pretty print
 *)

open Format

let tab_width = 2

let rec print_t pl =
  open_box tab_width;
  print_t' pl;
  close_box ();
  print_newline ();
and print_t' pl =
  print_process pl.loc_val
and print_process p =
  begin
    match p with
    | PNil ->
       print_string "O"
    | PIn(body) ->
       open_hbox ();
       print_string @@ body.x ^ "?";
       print_names body.ys;
       print_string ".";
       print_cut ();
       print_t' body.pl;
       close_box ()
    | PRIn(body) ->
       open_hovbox 3;
       print_string @@ "(*" ^ body.x ^ "?";
       print_names body.ys;
       print_string ".";
       print_cut ();
       print_t' body.pl;
       print_string ")";
       close_box ()
    | POut(body) ->
       open_hovbox tab_width;
       print_string @@ body.x ^ "!";
       print_es body.els;
       print_string ".";
       print_cut ();
       print_t' body.pl;
       close_box ()
    | PPar(body) ->
       let rec print_ppar pl1 pl2 =
	 open_vbox 0;
	 (match pl1.loc_val with
	  | PPar(body) ->
	     print_ppar body.pl1 body.pl2
	  | _ ->
	     print_t' pl1);
	 print_cut ();
	 print_string "| ";
	 (match pl2.loc_val with
	  | PPar(body) ->
	     print_ppar body.pl1 body.pl2
	  | _ ->
	     print_t' pl2);
	 close_box ();
       in
       open_box 1;
       print_string "(";
       print_ppar body.pl1 body.pl2;
       print_string ")";
       close_box ();
    | PRes(body) ->
       let rec print_pres x tyxo pl =
	 print_string @@ "new ";
	 (match tyxo with
	  | None ->
	     print_string x
	  | Some(tyx) ->
	     (print_string @@ "(" ^ x;
	      print_string " : ";
	      Type.print_t tyx;
	      print_string ")"));
	 print_string " in";
	 (match pl.loc_val with
	  | PRes(body) ->
	     (print_space ();
	      print_pres body.x body.tyxo body.pl)
	  | _ ->
	     ())
       in
       open_vbox tab_width;
       open_box 0;
       print_pres body.x body.tyxo body.pl;
       close_box ();
       print_cut ();
       open_box 0;
       print_t' body.pl;
       close_box ();
       close_box ()
    | PIf(body) ->
       open_vbox 0;
       (* then clause *)
       open_vbox tab_width;
       print_string "if ";
       print_e body.el;
       print_string " then";
       print_space ();
       close_box ();
       open_box 0;
       print_t' body.pl1;
       close_box ();
       (* else clause *)
       print_space ();
       open_vbox tab_width;
       print_string "else";
       print_space ();
       open_box 0;
       print_t' body.pl2;
       close_box ();
       close_box ();
       close_box ()
  end;
and print_names ys =
  let n = List.length ys in
  if n = 0 then
    (eprintf "Warning: no arguments for channel input";
     print_string "()")
  else if n = 1 then
    print_string @@ List.hd ys
  else
    (open_hovbox 1;
     print_string "(";
     print_names' ys;
     print_string ")";
     close_box ())
and print_names' ys =
  match ys with
  | [] -> ()
  | y :: ys ->
     print_string y;
     List.iter (fun y -> print_string ","; print_space (); print_string y;) ys
and print_es els =
  let n = List.length els in
  if n = 0 then
    print_string "()"
  else if n = 1 then
    let el = List.hd els in
    match el.loc_val with
    | EVar(_) | EUnit | EBool(_) | EInt(_)
    | ENot(_) | ENeg(_) ->
       print_e el
    | _ ->
       (open_box 1;
	print_string "(";
	print_e el;
	print_string ")";
	close_box ())
  else
    (open_hovbox 1;
     print_string "(";
     print_es' els;
     print_string ")";
     close_box ())
and print_es' els =
  match els with
  | [] -> ()
  | el :: els ->
     print_e el;
     List.iter (fun el -> (print_string ","; print_space (); print_e el)) els
and print_e el =
  print_expr el.loc_val
and print_expr e =
  match e with
  | EVar(x) ->
     print_string x
  | EUnit ->
     print_string "()"
  | EBool(b) ->
     print_bool b
  | EInt(i) ->
     print_int i
  | ENot(el) ->
     print_unary_expr "not" el
  | ENeg(el) ->
     print_unary_expr "-" el
  | EAnd(el1, el2) ->
     print_binary_expr "and" el1 el2
  | EOr(el1, el2) ->
     print_binary_expr "or" el1 el2
  | EAdd(el1, el2) ->
     print_binary_expr "+" el1 el2
  | ESub(el1, el2) ->
     print_binary_expr "-" el1 el2
  | EMul(el1, el2) ->
     print_binary_expr "*" el1 el2
  | EDiv(el1, el2) ->
     print_binary_expr "/" el1 el2
  | EEq(el1, el2) ->
     print_binary_expr "=" el1 el2
  | ELt(el1, el2) ->
     print_binary_expr "<" el1 el2
  | EGt(el1, el2) ->
     print_binary_expr ">" el1 el2
  | ELeq(el1, el2) ->
     print_binary_expr "<=" el1 el2
  | EGeq(el1, el2) ->
     print_binary_expr ">=" el1 el2
and print_unary_expr (op : string) el =
  open_box @@ String.length op + 1;
  print_string "(";
  print_string op;
  print_e el;
  print_string ")";
  close_box ()
and print_binary_expr (op : string) el1 el2 =
  open_box 0;
  print_e el1;
  print_string @@ " " ^ op;
  print_space ();
  print_e el2;
  close_box ()
