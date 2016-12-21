(* simple type inference *)

open Syntax
open Location
open Type
open Sbst

exception UnifyT of Type.t * Type.t
exception UnifyR of Type.region * Type.region

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

(* unifyT : ConstraintsT.t -> Type.t sbst *)
let rec unifyT ct =
  if ct = ConstraintsT.empty then []
  else
    let tt = ConstraintsT.choose ct in
    let ct = ConstraintsT.remove tt ct in
    match tt with
    | (ty1, ty2) when ty1 = ty2 ->
       unifyT ct
    | ((TChan(_, tys1, ro1) as ty1), (TChan(_, tys2, ro2) as ty2)) ->
       if List.length tys1 = List.length tys2 then
	 let ct = List.fold_left2
		    (fun ct ty1 ty2 -> ConstraintsT.add (ty1, ty2) ct)
		    ct tys1 tys2
	 in unifyT ct
       else
	 raise @@ UnifyT(ty1, ty2)
    | (TVar(a), ty2) when not @@ Type.contain a ty2 ->
       let ct = ConstraintsT.sbst [(a, ty2)] ct in
       Sbst.compose (unifyT ct) [(a, ty2)]
    | (ty1, TVar(a)) when not @@ Type.contain a ty1 ->
       let ct = ConstraintsT.sbst [(a, ty1)] ct in
       Sbst.compose (unifyT ct) [(a, ty1)]
    | (ty1, ty2) ->
       raise @@ UnifyT(ty1, ty2)

(* unifyR : ConstraintsR.t -> Type.region sbst *)
let rec unifyR cr =
  if cr = ConstraintsR.empty then []
  else
    let rr = ConstraintsR.choose cr in
    let cr = ConstraintsR.remove rr cr in
    if fst rr = snd rr then
      unifyR cr
    else
      let cr = ConstraintsR.sbst [rr] cr in
      Sbst.compose (unifyR cr) [rr]

(* Gather constraints while checking that two types are the same shape *)
(* eq_type_pattern : ConstraintsT.t -> ConstraintsR.t -> Type.t -> Type.t ->
                     bool * ConstraintsT.t * ConstraintsR.t *)
let rec eq_type_pattern ct cr ty1 ty2 =
  match ty1, ty2 with
  | TChan(_, tys1, ro1), TChan(_, tys2, ro2) ->
     begin
       match ro1, ro2 with
       | Some(r1), Some(r2) ->
	  let (b, ct, cr) = eq_type_patterns ct cr tys1 tys2 in
	  (b, ct, ConstraintsR.add (r1, r2) cr)
       | _, _ ->
	  (false, ct, cr)
     end
  | TUnit, TUnit
  | TBool, TBool
  | TInt, TInt ->
     (true, ct, cr)
  | TVar(_), TVar(_) ->
     (true, ConstraintsT.add (ty1, ty2) ct, cr)
  | _, _ ->
     (false, ct, cr)
and eq_type_patterns ct cr tys1 tys2 =
  match tys1, tys2 with
  | [], [] -> (true,  ct, cr)
  | [], _  -> (false, ct, cr)
  | _, []  -> (false, ct, cr)
  | ty1 :: tys1, ty2 :: tys2 ->
     let (b, ct, cr) = eq_type_pattern ct cr ty1 ty2 in
     if b then
       eq_type_patterns ct cr tys1 tys2
     else
       (false, ct, cr)

(* infer_expr : Env.key -> Syntax.e -> Type.e * ConstraintsT.t * ConstraintsR.t *)
let rec infer_expr env el =
  match el.loc_val with
  | EVar(x) ->
     if Env.mem x env then
       (Env.find x env, ConstraintsT.empty, ConstraintsR.empty)
     else
       let ty = TVar(gensym_type ()) in
       (ty, ConstraintsT.empty, ConstraintsR.empty)
  | EUnit ->
     (TUnit, ConstraintsT.empty, ConstraintsR.empty)
  | EBool(b) ->
     (TBool, ConstraintsT.empty, ConstraintsR.empty)
  | EInt(i) ->
     (TInt, ConstraintsT.empty, ConstraintsR.empty)
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
     let (ty1, ct1, cr1) = infer_expr env el1 in
     let (ty2, ct2, cr2) = infer_expr env el2 in
     let ct = ConstraintsT.union ct1 ct2 in
     let cr = ConstraintsR.union cr1 cr2 in
     begin
       match ty1, ty2 with
       | TVar(_), _
       | _, TVar(_) ->
	  let ct = ConstraintsT.add (ty1, ty2) ct in
	  (TBool, ct, cr)
       | _, _ ->
	  let (b, ct, cr) = eq_type_pattern ct cr ty1 ty2 in
	  if b then
	    (TBool, ct, cr)
	  else
	    raise @@ TypeErr(sprint_expr_error el2 ty2 ty1)
     end
and infer_unary_expr env el1 tyin tyout =
  let (ty, ct, cr) = infer_expr env el1 in
  match ty with
  | _ when ty = tyin ->
     (tyout, ct, cr)
  | TVar(_) ->
     (tyout, ConstraintsT.add (ty, tyin) ct, cr)
  | _ ->
     raise @@ TypeErr(sprint_expr_error el1 ty tyin)
and infer_binary_expr env el1 el2 tyin tyout =
  let (ty1, ct1, cr1) = infer_expr env el1 in
  let (ty2, ct2, cr2) = infer_expr env el2 in
  let ct = ConstraintsT.union ct1 ct2 in
  let cr = ConstraintsR.union cr1 cr2 in
  match ty1, ty2 with
  | _, _ when ty1 = tyin && ty2 = tyin ->
     (tyout, ct, cr)
  | TVar(_), _ when ty2 = tyin ->
     (tyout, ConstraintsT.add (ty1, tyin) ct, cr)
  | _, TVar(_)  when ty1 = tyin ->
     (tyout, ConstraintsT.add (ty2, tyin) ct, cr)
  | TVar(_), TVar(_) ->
     (tyout, ConstraintsT.add (ty1, tyin)
			      (ConstraintsT.add (ty2, tyin) ct), cr)
  | TVar(_), _ ->
     raise @@ TypeErr(sprint_expr_error el2 ty2 tyin)
  | _, _ ->
     raise @@ TypeErr(sprint_expr_error el1 ty1 tyin)

(* infer_proc : Syntax.t -> Env.key -> ConstraintsT.t * ConstraintsR.t *)
let rec infer_proc env pl =
  match pl.loc_val with
  | PNil ->
     (ConstraintsT.empty, ConstraintsR.empty)
  | PIn(body)
  | PRIn(body) ->
     infer_proc_input env body pl.loc_start pl.loc_end
  | POut(body) ->
     let pack e = { loc_val = e; loc_start = pl.loc_start; loc_end = pl.loc_end } in
     let fst (x, _, _) = x in
     let snd (_, y, _) = y in
     let trd (_, _, z) = z in
     if Env.mem body.x env then
       let ty = Env.find body.x env in
       match ty with
       | TChan(_, tys, _) ->
	  (* Check (types of es) = tys *)
	  let tyccs = List.map (infer_expr env) body.els in
	  let tys' = List.map fst tyccs in
	  let (b, ct', cr') = eq_type_patterns ConstraintsT.empty ConstraintsR.empty
					       tys tys' in
	  if b then
	    begin
	      let (ct', cr') = 
		List.fold_left
		  (fun (ct, cr) tycc -> (ConstraintsT.union (snd tycc) ct,
					 ConstraintsR.union (trd tycc) cr))
		  (ct', cr') tyccs in
	      body.tyxo <- Some(ty);
	      let (ct, cr) = infer_proc env body.pl in
	      (ConstraintsT.union ct ct',
	       ConstraintsR.union cr cr')
	    end
	  else
	    raise @@ TypeErr(sprint_error_chan_args body.x body.els tys' tys)
       | TVar(a) ->
	  (* Compose constraints *)
	  let tyccs = List.map (infer_expr env) body.els in
	  let tys = List.map fst tyccs in
	  let (ct, cr) = infer_proc env body.pl in
	  let (ct, cr) =
	    List.fold_left
	      (fun (ct, cr) tycc -> (ConstraintsT.union (snd tycc) ct,
				     ConstraintsR.union (trd tycc) cr))
	      (ct, cr) tyccs in
	  body.tyxo <- Some(ty);
	  (ConstraintsT.add (ty, TChan(None, tys, Some(gensym_region ()))) ct,
	   cr)
       | _ ->
	  raise @@ TypeErr(sprint_expr_error_chan (pack @@ EVar(body.x)) ty)
     else
       (* Compose constraints *)
       let tyccs = List.map (infer_expr env) body.els in
       let tys = List.map fst tyccs in
       let ct1 = List.fold_left (fun ct1 ct2 -> ConstraintsT.union ct1 ct2)
				ConstraintsT.empty
				(List.map snd tyccs) in
       let cr1 = List.fold_left (fun cr1 cr2 -> ConstraintsR.union cr1 cr2)
				ConstraintsR.empty
				(List.map trd tyccs) in
       let ty = TVar(gensym_type ()) in
       let ty' = TChan(None, tys, Some(gensym_region ())) in
       let env = Env.add body.x ty' env in
       body.tyxo <- Some(ty);
       let (ct2, cr2) = infer_proc env body.pl in
       (ConstraintsT.add (ty, ty') (ConstraintsT.union ct1 ct2),
	ConstraintsR.union cr1 cr2)
  | PPar(body) ->
     let (ct1, cr1) = infer_proc env body.pl1 in
     let (ct2, cr2) = infer_proc env body.pl2 in
     (ConstraintsT.union ct1 ct2, ConstraintsR.union cr1 cr2)
  | PRes(body) ->
     let ty = TVar(gensym_type ()) in
     let env = Env.add body.x ty env in
     body.tyxo <- Some(ty);
     infer_proc env body.pl
  | PIf(body) ->
     let (tye, ct0, cr0) = infer_expr env body.el in
     begin
       match tye with
       | TBool ->
	  let (ct1, cr1) = infer_proc env body.pl1 in
	  let (ct2, cr2) = infer_proc env body.pl2 in
	  (ConstraintsT.union ct0 (ConstraintsT.union ct1 ct2),
	   ConstraintsR.union cr0 (ConstraintsR.union cr1 cr2))
       | _ ->
	  raise @@ TypeErr(sprint_expr_error body.el tye TBool)
     end
and infer_proc_input env body loc_start loc_end =
  let pack e = { loc_val = e; loc_start = loc_start; loc_end = loc_end } in
  if Env.mem body.x env then
    let ty = Env.find body.x env in
    match ty with
    | TChan(_, tys, _) ->
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
       let (ct, cr) = infer_proc env body.pl in
       (ConstraintsT.add (ty, ty') ct, cr)
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
    let (ct, cr) = infer_proc env body.pl in
    (ConstraintsT.add (ty, ty') ct, cr)

(* annotate : Type.t sbst -> Type.region sbst -> Syntax.t -> unit *)
let rec annotate sigmaT sigmaR pl =
  (* TODO *)
  match pl.loc_val with
  | PNil -> ()
  | PIn(body)
  | PRIn(body) ->
     body.tyxo <- annotate_type_option sigmaT sigmaR body.tyxo;
     body.tyyos <- List.map (annotate_type_option sigmaT sigmaR) body.tyyos;
     annotate sigmaT sigmaR body.pl
  | POut(body) ->
     body.tyxo <- annotate_type_option sigmaT sigmaR body.tyxo;
     annotate sigmaT sigmaR body.pl
  | PRes(body) ->
     body.tyxo <- annotate_type_option sigmaT sigmaR body.tyxo;
     annotate sigmaT sigmaR body.pl
  | PPar(body) ->
     annotate sigmaT sigmaR body.pl1;
     annotate sigmaT sigmaR body.pl2
  | PIf(body) ->
     annotate sigmaT sigmaR body.pl1;
     annotate sigmaT sigmaR body.pl2
and annotate_type_option (sigmaT : Type.t sbst) (sigmaR : Type.region sbst) tyo =
  match tyo with
  | Some(ty) -> Some(annotate_type sigmaT sigmaR ty)
  | None -> None
and annotate_type (sigmaT : Type.t sbst) (sigmaR : Type.region sbst) ty =
  match ty with
  | TVar(a) ->
     begin
       try
	 let (_, ty) = List.find (fun (a', ty') -> a' = a) sigmaT in
	 annotate_type sigmaT sigmaR ty
       with
       | Not_found ->
	  (* TODO(nekketsuuu): 同じ型変数に対しては1回だけ出力するようにする? *)
	  (Printf.eprintf "Warning: An uninstantiated type variable %s is assumed type int\n%!" a;
	   TInt)
     end
  | TChan(lo, tys, ro) ->
     (match ro with
      | Some(r) ->
	 (try
	     let (_, r) = List.find (fun (r', r'') -> r' = r) sigmaR in
	     TChan(lo, List.map (annotate_type sigmaT sigmaR) tys, Some(r))
	   with
	   | Not_found ->
	      TChan(lo, List.map (annotate_type sigmaT sigmaR) tys, ro))
      | None ->
	 raise @@ TypeErr("Unexpected error at annotate_type: region is None"))
  | ty -> ty

(* infer : Syntax.t -> unit *)
let infer pl =
  let (ct, cr) = infer_proc Env.empty pl in
  try
    let sigmaT = unifyT ct in
    let sigmaR = unifyR cr in
    annotate sigmaT sigmaR pl
  with
  | UnifyT(ty1, ty2) ->
     raise @@ TypeErr("TODO(nekketsuuu): type unify error")
