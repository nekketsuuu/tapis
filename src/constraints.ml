module Constraints =
  Set.Make
    (struct
	type t = Type.t * Type.t
	let compare = compare
      end)
include Constraints

(* sbst : (string * Type.t) list -> t -> t *)
let sbst sigma c =
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
