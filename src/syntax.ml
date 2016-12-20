open Location

exception LexErr of string
exception ParseErr of string
exception EvalErr of string
exception TypeErr of string

let app_name = "piterm"

type name    = string
and  an_name = string * Type.t option
[@@deriving show]
  

type t = process location
and process =
  | PNil
  | PIn   of an_name * an_name list * t
  | POut  of an_name * e list * t
  | PRIn  of an_name * an_name list * t
  | PPar  of t * t
  | PRes  of an_name * t
  | PIf   of e * t * t
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
  | PIn(xtyo, ytyos, pl)
  | PRIn(xtyo, ytyos, pl) ->
     let bounded_names' = union (List.map fst ytyos) bounded_names in
     let free_names = free_name_b bounded_names' pl in
     let x = fst xtyo in
     let free_names =
       if List.mem x bounded_names then
	 free_names
       else
	 add x free_names
     in
     List.fold_left
       (fun names (y, tyo) -> remove y names)
       free_names
       ytyos
  | POut(xtyo, els, pl) ->
     let names = List.fold_left
		   (fun names el -> union (name_expr el) names)
		   []
		   els in
     let free_names = diff names bounded_names in
     let free_names' = free_name_b bounded_names pl in
     let free_names = union free_names free_names' in
     let x = fst xtyo in
     if List.mem x bounded_names then
       free_names
     else
       add x free_names
  | PPar(pl1, pl2) ->
     union (free_name_b bounded_names pl1)
	   (free_name_b bounded_names pl2)
  | PRes(xtyo, pl) ->
     let x = fst xtyo in
     remove x (free_name_b (add x bounded_names) pl)
  | PIf(el, pl1, pl2) ->
     let names = name_expr el in
     let free_names = diff names bounded_names in
     let free_names' = union (free_name_b bounded_names pl1)
			     (free_name_b bounded_names pl2) in
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

let closure pl =
  let free_names = free_name pl in
  let restrict pl name = 
    (Printf.eprintf "Warning: A free name %s is restricted globally\n%!" name;
     Location.dummy_loc @@ PRes((name, None), pl)) in
  List.fold_left restrict pl free_names
