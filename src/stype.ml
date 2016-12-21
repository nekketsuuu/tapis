(* simple type inference *)

open Syntax
open Location
open Type
open Sbst

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

(* unify : Constraints.t -> Type.t sbst *)
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
		     c' tys1 tys2
	 in unify c''
       else
	 raise @@ Unify(ty1, ty2)
    | (TVar(a), ty2) when not @@ Type.contain a ty2 ->
       let c'' = Constraints.sbst [(a, ty2)] c' in
       Sbst.compose (unify c'') [(a, ty2)]
    | (ty1, TVar(a)) when not @@ Type.contain a ty1 ->
       let c'' = Constraints.sbst [(a, ty1)] c' in
       Sbst.compose (unify c'') [(a, ty1)]
    | (ty1, ty2) ->
       raise @@ Unify(ty1, ty2)

(* 型の形が等しいか確認しながら制約を集める *)
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

(* infer_proc : Syntax.t -> Env.key -> Constraints.t *)
let rec infer_proc env pl =
  match pl.loc_val with
  | PNil ->
     Constraints.empty
  | PIn(body)
  | PRIn(body) ->
     infer_proc_input env body pl.loc_start pl.loc_end
  | POut(body) ->
     let pack e = { loc_val = e; loc_start = pl.loc_start; loc_end = pl.loc_end } in
     if Env.mem body.x env then
       let ty = Env.find body.x env in
       match ty with
       | TChan(l, tys, r) ->
	  (* Check (types of es) = tys *)
	  let tycs = List.map (infer_expr env) body.els in
	  let tys' = List.map fst tycs in
	  let (b, c') = eq_type_patterns Constraints.empty tys tys' in
	  if b then
	    begin
	      let c' = List.fold_left (fun c tyc -> Constraints.union (snd tyc) c)
				      c' tycs in
	      body.tyxo <- Some(ty);
	      Constraints.union (infer_proc env body.pl) c'
	    end
	  else
	    raise @@ TypeErr(sprint_error_chan_args body.x body.els tys' tys)
       | TVar(a) ->
	  (* Make a constraint *)
	  let tycs = List.map (infer_expr env) body.els in
	  let tys = List.map fst tycs in
	  let c = infer_proc env body.pl in
	  let c = List.fold_left (fun c tyc -> Constraints.union (snd tyc) c)
				 c tycs in
	  body.tyxo <- Some(ty);
	  Constraints.add (ty, TChan(None, tys, Some(gensym_region ()))) c
       | _ ->
	  raise @@ TypeErr(sprint_expr_error_chan (pack @@ EVar(body.x)) ty)
     else
       let tycs = List.map (infer_expr env) body.els in
       let tys = List.map fst tycs in
       let c1 = List.fold_left (fun c1 c2 -> Constraints.union c1 c2)
			       Constraints.empty
			       (List.map snd tycs) in
       let ty = TVar(gensym_type ()) in
       let ty' = TChan(None, tys, Some(gensym_region ())) in
       let env = Env.add body.x ty' env in
       body.tyxo <- Some(ty);
       let c2 = infer_proc env body.pl in
       Constraints.add (ty, ty') (Constraints.union c1 c2)
  | PPar(body) ->
     let c1 = infer_proc env body.pl1 in
     let c2 = infer_proc env body.pl2 in
     Constraints.union c1 c2
  | PRes(body) ->
     let ty = TVar(gensym_type ()) in
     let env = Env.add body.x ty env in
     body.tyxo <- Some(ty);
     infer_proc env body.pl
  | PIf(body) ->
     let (tye, c0) = infer_expr env body.el in
     begin
       match tye with
       | TBool ->
	  let c1 = infer_proc env body.pl1 in
	  let c2 = infer_proc env body.pl2 in
	  Constraints.union c0 (Constraints.union c1 c2)
       | _ ->
	  raise @@ TypeErr(sprint_expr_error body.el tye TBool)
     end
and infer_proc_input env body loc_start loc_end =
  let pack e = { loc_val = e; loc_start = loc_start; loc_end = loc_end } in
  if Env.mem body.x env then
    let ty = Env.find body.x env in
    match ty with
    | TChan(l, tys, r) ->
       let env = List.fold_left2
		   (fun env y ty -> Env.add y ty env)
		   env body.ys tys in
       body.tyxo <- Some(ty);
       body.tyyos <- List.map (fun ty -> Some(ty)) tys;
       infer_proc env body.pl
    | TVar(a) ->
       let tys = List.map (fun _ -> TVar(gensym_type ())) body.ys in
       let ty' = TChan(None, tys, Some(gensym_region ())) in
       let env = Env.add body.x ty' env in
       let env = List.fold_left2
		   (fun env y ty -> Env.add y ty env)
		   env body.ys tys in
       body.tyxo <- Some(ty);
       body.tyyos <- List.map (fun ty -> Some(ty)) tys;
       Constraints.add (ty, ty') (infer_proc env body.pl)
    | _ ->
       raise @@ TypeErr(sprint_expr_error_chan (pack @@ EVar(body.x)) ty)
  else
    let ty = TVar(gensym_type ()) in
    let tys = List.map (fun _ -> TVar(gensym_type ())) body.ys in
    let ty' = TChan(None, tys, Some(gensym_region ())) in
    let env = Env.add body.x ty' env in
    let env = List.fold_left2
		(fun env y ty -> Env.add y ty env)
		env body.ys tys in
    Constraints.add (ty, ty') (infer_proc env body.pl)

(* annotate : Type.t sbst -> Syntax.t -> unit *)
let rec annotate sigma pl =
  match pl.loc_val with
  | PNil -> ()
  | PIn(body)
  | PRIn(body) ->
     body.tyxo <- annotate_type_option sigma body.tyxo;
     body.tyyos <- List.map (annotate_type_option sigma) body.tyyos;
     annotate sigma body.pl
  | POut(body) ->
     body.tyxo <- annotate_type_option sigma body.tyxo;
     annotate sigma body.pl
  | PRes(body) ->
     body.tyxo <- annotate_type_option sigma body.tyxo;
     annotate sigma body.pl
  | PPar(body) ->
     annotate sigma body.pl1;
     annotate sigma body.pl2
  | PIf(body) ->
     annotate sigma body.pl1;
     annotate sigma body.pl2
and annotate_type_option (sigma : Type.t sbst) tyo =
  match tyo with
  | Some(ty) -> Some(annotate_type sigma ty)
  | None -> None
and annotate_type (sigma : Type.t sbst) ty =
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

(* infer : Syntax.t -> unit *)
let infer pl =
  let c = infer_proc Env.empty pl in
  try
    let sigma = unify c in
    annotate sigma pl
  with
  | Unify(ty1, ty2) ->
     raise @@ TypeErr("TODO(nekketsuuu): type unify error")
