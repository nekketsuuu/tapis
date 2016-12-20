(* simple type inference *)

open Syntax
open Location
open Type

exception Unify of Type.t * Type.t

(* sprint_expr_error : Syntax.t -> Type.t -> Type.t -> string *)
let sprint_expr_error el actual_ty expected_ty =
  (* TODO(nekketsuuu): 型のpretty print *)
  (* TODO(nekketsuuu): print location *)
  Printf.sprintf
    "An expression %s has type %s but an expression was expected of type %s"
    (Syntax.show_expr el.loc_val)
    (Type.show actual_ty)
    (Type.show expected_ty)
let sprint_expr_error_chan el actual_ty =
  Printf.sprintf
    "An expression %s has type %s but an expression was expected of channel type"
    (Syntax.show_expr el.loc_val)
    (Type.show actual_ty)
let rec sprint_error_chan_args x els actual_tys expected_tys =
  match actual_tys, expected_tys with
  | [], [] ->
     "Unexpected error at sprint_error_chan_args"
  | [], _ ->
     Printf.sprintf
       "The number of channel arguments of %s is greater than expected"
       x
  | _, [] ->
     Printf.sprintf
       "The number of channel arguments of %s is less than expected"
       x
  (* TODO(nekketsuuu): aty != ety じゃなくて形で判断させたい *)
  | (aty :: atys), (ety :: etys) when aty != ety ->
     Printf.sprintf
       "Channel arguments %s has type %s but an expression was expected of type %s"
       (Syntax.show_expr (List.hd els).loc_val)
       (Type.show aty)
       (Type.show ety)
  | (aty :: atys), (ety :: etys) ->
     sprint_error_chan_args x (List.tl els) atys etys

type sbst_t = (string * Type.t) list
let compose sigma1 sigma2 = List.append sigma1 sigma2

(* unify : Constraints.t -> sbst_t *)
let rec unify c = 
  if c = Constraints.empty then []
  else
    let tt = Constraints.choose c in
    let c' = Constraints.remove tt c in
    match tt with
    | (ty1, ty2) when ty1 = ty2 ->
       unify c'
    | ((TChan(l1, tys1, r1) as ty1), (TChan(l2, tys2, r2) as ty2)) ->
       if List.length tys1 = List.length tys2 then
	 let c'' = List.fold_left2
		     (fun c ty1 ty2 -> Constraints.add (ty1, ty2) c)
		     c'
		     tys1
		     tys2
	 in
	 unify c''
       else
	 raise @@ Unify(ty1, ty2)
    | (TVar(a), ty2) when not @@ Type.contain a ty2 ->
       let c'' = Constraints.sbst [(a, ty2)] c' in
       compose (unify c'') [(a, ty2)]
    | (ty1, TVar(a)) when not @@ Type.contain a ty1 ->
       let c'' = Constraints.sbst [(a, ty1)] c' in
       compose (unify c'') [(a, ty1)]
    | (ty1, ty2) ->
       raise @@ Unify(ty1, ty2)

(* eq_type_pattern : Constraints.t -> Type.t -> Type.t -> bool * Constraints.t *)
let rec eq_type_pattern c ty1 ty2 =
  match ty1, ty2 with
  | TChan(_, tys1, _), TChan(_, tys2, _) ->
     eq_type_patterns c tys1 tys2
  | TUnit, TUnit
  | TBool, TBool
  | TInt, TInt ->
     (true, c)
  | TVar(_), TVar(_) ->
     (true, Constraints.add (ty1, ty2) c)
  | _, _ ->
     (false, c)
and eq_type_patterns c tys1 tys2 =
  match tys1, tys2 with
  | [], [] -> (true,  c)
  | [], _  -> (false, c)
  | _, []  -> (false, c)
  | ty1 :: tys1, ty2 :: tys2 ->
     let (b, c) = eq_type_pattern c ty1 ty2 in
     if b then
       eq_type_patterns c tys1 tys2
     else
       (false, c)

(* infer_expr : Env.key -> Syntax.e -> Type.e * Constraints.t *)
let rec infer_expr env el =
  match el.loc_val with
  | EVar(x) ->
     if Env.mem x env then
       (Env.find x env, Constraints.empty)
     else
       let ty = TVar(gensym_type ()) in
       (ty, Constraints.empty)
  | EUnit ->
     (TUnit, Constraints.empty)
  | EBool(b) ->
     (TBool, Constraints.empty)
  | EInt(i) ->
     (TInt, Constraints.empty)
  | ENot(el1) ->
     infer_unary_expr env el1 TBool TBool
  | EAnd(el1, el2)
  | EOr(el1, el2) ->
     infer_binary_expr env el1 el2 TBool TBool
  | ENeg(el1) ->
     infer_unary_expr env el1 TInt TInt
  | EAdd(el1, el2)
  | ESub(el1, el2)
  | EMul(el1, el2)
  | EDiv(el1, el2) ->
     infer_binary_expr env el1 el2 TInt TInt
  | ELt(el1, el2)
  | EGt(el1, el2)
  | ELeq(el1, el2)
  | EGeq(el1, el2) ->
     infer_binary_expr env el1 el2 TInt TBool
  | EEq(el1, el2) ->
     let (ty1, c1) = infer_expr env el1 in
     let (ty2, c2) = infer_expr env el2 in
     let c = Constraints.union c1 c2 in
     begin
       match ty1, ty2 with
       | TVar(_), _
       | _, TVar(_) ->
	  let c = Constraints.add (ty1, ty2) c in
	  (TBool, c)
       | _, _ when ty1 = ty2 ->
	  (TBool, c)
       | _, _ ->
	  raise @@ TypeErr(sprint_expr_error el2 ty2 ty1)
     end
and infer_unary_expr env el1 tyin tyout =
  let (ty, c) = infer_expr env el1 in
  match ty with
  | _ when ty = tyin ->
     (tyout, c)
  | TVar(_) ->
     (tyout, Constraints.singleton (ty, tyin))
  | _ ->
     raise @@ TypeErr(sprint_expr_error el1 ty tyin)
and infer_binary_expr env el1 el2 tyin tyout =
  let (ty1, c1) = infer_expr env el1 in
  let (ty2, c2) = infer_expr env el2 in
  let c = Constraints.union c1 c2 in
  match ty1, ty2 with
  | _, _ when ty1 = tyin && ty2 = tyin ->
     (tyout, c)
  | TVar(_), _ when ty2 = tyin ->
     (tyout, Constraints.add (ty1, tyin) c)
  | _, TVar(_)  when ty1 = tyin ->
     (tyout, Constraints.add (ty2, tyin) c)
  | TVar(_), TVar(_) ->
     (tyout, Constraints.add (ty1, tyin)
			     (Constraints.add (ty2, tyin) c))
  | TVar(_), _ ->
     raise @@ TypeErr(sprint_expr_error el2 ty2 tyin)
  | _, _ ->
     raise @@ TypeErr(sprint_expr_error el1 ty1 tyin)

(* infer_proc : Syntax.t -> Env.key -> Syntax.t * Constraints.t *)
let rec infer_proc env pl =
  (* TODO(nekketsuuu): 後でrefにする *)
  let pack p = { loc_val = p; loc_start = pl.loc_start; loc_end = pl.loc_end } in
  match pl.loc_val with
  | PNil ->
     (pack PNil, Constraints.empty)
  | PIn(xtyo, ytyos, pl) ->
     infer_proc_input env xtyo ytyos pl
		      (fun xtyo ytyos pl -> PIn(xtyo, ytyos, pl))
		      pl.loc_start pl.loc_end
  | PRIn(xtyo, ytyos, pl) ->
     infer_proc_input env xtyo ytyos pl
		      (fun xtyo ytyos pl -> PRIn(xtyo, ytyos, pl))
		      pl.loc_start pl.loc_end
  | POut(xtyo, els, pl) ->
     infer_proc_output env xtyo els pl
		       (fun xtyo els pl -> POut(xtyo, els, pl))
		       pl.loc_start pl.loc_end
  | PPar(pl1, pl2) ->
     let (pl1, c1) = infer_proc env pl1 in
     let (pl2, c2) = infer_proc env pl2 in
     (pack @@ PPar(pl1, pl2), Constraints.union c1 c2)
  | PRes(xtyo, pl) ->
     let ty = TVar(gensym_type ()) in
     let env = Env.add (fst xtyo) ty env in
     let (pl, c) = infer_proc env pl in
     (pack @@ PRes((fst xtyo, Some(ty)), pl), c)
  | PIf(el, pl1, pl2) ->
     let (tye, c0) = infer_expr env el in
     match tye with
     | TBool ->
	let (pl1, c1) = infer_proc env pl1 in
	let (pl2, c2) = infer_proc env pl2 in
	let c = Constraints.union c0 (Constraints.union c1 c2) in
	(pack @@ PIf(el, pl1, pl2), c)
     | _ ->
	raise @@ TypeErr(sprint_expr_error el tye TBool)
and infer_proc_input env xtyo ytyos pl to_type loc_start loc_end =
  let pack e = { loc_val = e; loc_start = loc_start; loc_end = loc_end } in
  if Env.mem (fst xtyo) env then
    let ty = Env.find (fst xtyo) env in
    match ty with
    | TChan(l, tys, r) ->
       (* TODO(nekketsuuu): 引数の型がenvにあるかもしれない *)
       let env = List.fold_left2
		   (fun env ytyo ty -> Env.add (fst ytyo) ty env)
		   env
		   ytyos
		   tys in
       let (pl, c) = infer_proc env pl in
       (pack @@ to_type (fst xtyo, Some(ty)) ytyos pl, c)
    | TVar(a) ->
       let tys = List.map (fun _ -> TVar(gensym_type ())) ytyos in
       let ty' = TChan(None, tys, Some(gensym_region ())) in
       let env = Env.add (fst xtyo) ty' env in
       let env = List.fold_left2
		   (fun env ytyo ty -> Env.add (fst ytyo) ty env)
		   env
		   ytyos
		   tys in
       let (pl, c) = infer_proc env pl in
       (pack @@ to_type (fst xtyo, Some(ty)) ytyos pl, Constraints.add (ty, ty') c)
    | _ ->
       raise @@ TypeErr(sprint_expr_error_chan (pack @@ EVar(fst xtyo)) ty)
  else
    let ty = TVar(gensym_type ()) in
    let tys = List.map (fun _ -> TVar(gensym_type ())) ytyos in
    let ty' = TChan(None, tys, Some(gensym_region ())) in
    let env = Env.add (fst xtyo) ty' env in
    let env = List.fold_left2
		(fun env ytyo ty -> Env.add (fst ytyo) ty env)
		env
		ytyos
		tys in
    let (pl, c) = infer_proc env pl in
    (pack @@ to_type (fst xtyo, Some(ty)) ytyos pl, Constraints.add (ty, ty') c)
and infer_proc_output env xtyo els pl to_type loc_start loc_end =
  let pack e = { loc_val = e; loc_start = loc_start; loc_end = loc_end } in
  if Env.mem (fst xtyo) env then
    let ty = Env.find (fst xtyo) env in
    match ty with
    | TChan(l, tys, r) ->
       (* Check (types of es) = tys *)
       let result = List.map (infer_expr env) els in
       let tys' = List.map fst result in
       let (b, c') = eq_type_patterns Constraints.empty tys tys' in
       let c' = List.fold_left (fun c tyc -> Constraints.union (snd tyc) c)
			       c' result in
       if b then
	 let (pl, c) = infer_proc env pl in
	 (pack @@ to_type (fst xtyo, Some(ty)) els pl, Constraints.union c c')
       else
	 raise @@ TypeErr(sprint_error_chan_args (fst xtyo) els tys' tys)
    | TVar(a) ->
       (* Make a constraint *)
       let result = List.map (infer_expr env) els in
       let tys = List.map fst result in
       let (pl, c) = infer_proc env pl in
       let c = List.fold_left (fun c tyc -> Constraints.union (snd tyc) c)
			      c result in
       (pack @@ to_type (fst xtyo, Some(ty)) els pl,
	Constraints.add (ty, TChan(None, tys, Some(gensym_region ()))) c)
    | _ ->
       raise @@ TypeErr(sprint_expr_error_chan (pack @@ EVar(fst xtyo)) ty)
  else
    let results = List.map (infer_expr env) els in
    let tys = List.map fst results in
    let c1 = List.fold_left (fun c1 c2 -> Constraints.union c1 c2)
			    Constraints.empty
			    (List.map snd results) in
    let ty = TVar(gensym_type ()) in
    let ty' = TChan(None,
		    tys,
		    Some(gensym_region ())) in
    let env = Env.add (fst xtyo) ty' env in
    let (pl, c2) = infer_proc env pl in
    let c = Constraints.add (ty, ty') (Constraints.union c1 c2) in
    (pack @@ to_type (fst xtyo, Some(ty)) els pl, c)

(* annotate : subst_t -> Syntax.t -> Syntax.t *)
let rec annotate sigma pl =
  let pl' =
    match pl.loc_val with
    | PNil ->
       PNil
    | PIn(xtyo, ytyos, pl) ->
       PIn(annotate_name sigma xtyo,
	   List.map (annotate_name sigma) ytyos,
	   annotate sigma pl)
    | PRIn(xtyo, ytyos, pl) ->
       PIn(annotate_name sigma xtyo,
	   List.map (annotate_name sigma) ytyos,
	   annotate sigma pl)
    | POut(xtyo, els, pl) ->
       POut(annotate_name sigma xtyo, els, annotate sigma pl)
    | PRes(xtyo, pl) ->
       PRes(annotate_name sigma xtyo, annotate sigma pl)
    | PPar(pl1, pl2) ->
       PPar(annotate sigma pl1, annotate sigma pl2)
    | PIf(e, pl1, pl2) ->
       PIf(e, annotate sigma pl1, annotate sigma pl2)
  in
  { loc_val = pl'; loc_start = pl.loc_start; loc_end = pl.loc_end }
and annotate_name (sigma : sbst_t) (x, tyo) =
  match tyo with
  | Some(ty) -> (x, Some(annotate_type sigma ty))
  | None -> (x, None)
and annotate_type (sigma : sbst_t) ty =
  match ty with
  | TVar(a) ->
     begin
       try
	 let (_, ty) = List.find (fun (a', ty') -> a' = a) sigma in
	 annotate_type sigma ty
       with
       | Not_found ->
	  (* TODO(nekketsuuu): 同じ型変数に対しては1回だけ出力するようにする? *)
	  (Printf.eprintf "Warning: An uninstantiated type variable %s is assumed type int\n%!" a;
	   TInt)
     end
  | TChan(l, tys, r) ->
     TChan(l, List.map (annotate_type sigma) tys, r)
  | ty -> ty

(* infer : Syntax.t -> Syntax.t *)
let infer pl =
  let (pl, c) = infer_proc Env.empty pl in
  try
    let sigma = unify c in
    annotate sigma pl
  with
  | Unify(ty1, ty2) ->
     raise @@ TypeErr("TODO(nekketsuuu): unify error")
