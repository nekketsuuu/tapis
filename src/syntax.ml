open Location

exception LexErr of string
exception ParseErr of string
exception EvalErr of string
exception TypeErr of string

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

(* free_name : t -> name list *)
let rec free_name pl = free_name_b [] pl
and free_name_b bounded_names pl =
  match pl.loc_val with
  | PNil -> []
  | PIn(body)
  | PRIn(body) ->
     let bounded_names' = union body.ys bounded_names in
     let free_names = free_name_b bounded_names' body.pl in
     let free_names =
       if List.mem body.x bounded_names then
	 free_names
       else
	 add body.x free_names
     in diff free_names body.ys
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
     remove body.x (free_name_b (add body.x bounded_names) body.pl)
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
