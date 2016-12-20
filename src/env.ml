module Env =
  Map.Make
    (struct
	type t = Syntax.name
	let compare = compare
      end)
include Env
