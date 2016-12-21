module ConstraintsR =
  Set.Make
    (struct
	type t = Type.region * Type.region
	let compare = compare
      end)
include ConstraintsR

open Sbst

(* sbst : Type.region sbst -> t -> t *)
let sbst (sigma : Type.region sbst) c =
  let sbst'' sigma r =
    try
      snd @@ List.find (fun (sym, r) -> sym = r) sigma
    with
    | Not_found -> r
  in
  let sbst' (r1, r2) =
    (sbst'' sigma r1, sbst'' sigma r2)
  in
  map sbst' c

(* for debug *)
let rec to_list c =
  if is_empty c then []
  else
    let elm = choose c in
    elm :: (to_list @@ remove elm c)
