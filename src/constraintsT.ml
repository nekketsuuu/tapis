module ConstraintsT =
  Set.Make
    (struct
	type t = Type.t * Type.t
	let compare = compare
      end)
include ConstraintsT

open Sbst

(* sbst : Type.t sbst -> t -> t *)
let sbst (sigma : Type.t sbst) c =
  let sbst' (t1, t2) =
    (Type.sbst sigma t1, Type.sbst sigma t2)
  in
  map sbst' c

(* for debug *)
let rec to_list c =
  if is_empty c then []
  else
    let elm = choose c in
    elm :: (to_list @@ remove elm c)
