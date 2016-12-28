module Env =
  Map.Make
    (struct
	type t = PiSyntax.name
	let compare = compare
      end)
include Env

open Error

let rec add_list ys tyys env =
  match ys, tyys with
  | [], [] -> env
  | y :: ys, tyy :: tyyos ->
     add_list ys tyyos (add y tyy env)
  | _, _ ->
     raise @@ ConvErr("add_list: Different length")
