module ConstraintsT =
  Set.Make
    (struct
	type t = Type.t * Type.t
	let compare = compare
      end)
include ConstraintsT

open Sbst

(* sbst : Type.t sbst -> t -> t *)
let sbst (sigma : Type.t sbst) ct =
  (*
  (*
   * Set.map has a bug in OCaml 4.04.0
   * https://caml.inria.fr/mantis/view.php?id=7403
   *)
  let sbst' (t1, t2) =
    (Type.sbst sigma t1, Type.sbst sigma t2)
  in
  map sbst' ct
   *)
  let sbst' (t1, t2) ct =
    ConstraintsT.add (Type.sbst sigma t1, Type.sbst sigma t2) ct
  in
  ConstraintsT.fold sbst' ct ConstraintsT.empty

(* for debug *)
let rec to_list c =
  (* print_endline @@ string_of_int @@ cardinal c; *)
  if ConstraintsT.is_empty c then []
  else
    let elm = ConstraintsT.choose c in
    elm :: (to_list @@ ConstraintsT.remove elm c)
