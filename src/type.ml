type level = int option
[@@deriving show]

type region = string option
[@@deriving show]

type t =
  | Chan of level * t list * region
  | Bool of bool
  | Int  of int
[@@deriving show]

let _type_i = ref 0
let gensym_type () =
  let name = "a" ^ string_of_int (!_type_i) in
  _type_i := !_type_i + 1;
  name

let _region_i = ref 0
let gensym_region () =
  let name = "r" ^ string_of_int (!_region_i) in
  _region_i := !_region_i + 1;
  name
