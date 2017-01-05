module ConstraintsR =
  Set.Make
    (struct
	type t = Type.region * Type.region
	let compare = compare
      end)
include ConstraintsR

open Sbst

(* sbst : Type.region sbst -> t -> t *)
let sbst (sigma : Type.region sbst) cr =
  let sbst'' sigma r =
    try
      List.assoc r sigma
    with
    | Not_found -> r
  in
  (*
  (*
   * Set.map has a bug in OCaml 4.04.0
   * https://caml.inria.fr/mantis/view.php?id=7403
   *)
  let sbst' (r1, r2) =
    (sbst'' sigma r1, sbst'' sigma r2)
  in
  map sbst' cr
   *)
  let sbst' (r1, r2) cr =
    ConstraintsR.add (sbst'' sigma r1, sbst'' sigma r2) cr
  in
  ConstraintsR.fold sbst' cr ConstraintsR.empty

(* for debug *)
let rec to_list c =
  if ConstraintsR.is_empty c then []
  else
    let elm = ConstraintsR.choose c in
    elm :: (to_list @@ ConstraintsR.remove elm c)
