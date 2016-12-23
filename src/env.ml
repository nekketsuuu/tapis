module Env =
  Map.Make
    (struct
	type t = PiSyntax.name
	let compare = compare
      end)
include Env
