type level = int option
[@@deriving show]

type region = string option
[@@deriving show]

type t =
  | TChan of level * t list * region
  | TUnit
  | TBool
  | TInt
  | TVar  of string
[@@deriving show]

let _type_i = ref 0
let gensym_type () =
  let name = "a" ^ string_of_int (!_type_i) in
  _type_i := !_type_i + 1;
  name
let rec gensym_type_list n = 
  if n <= 0 then []
  else gensym_type :: gensym_type_list (n-1)

let _region_i = ref 0
let gensym_region () =
  let name = "r" ^ string_of_int (!_region_i) in
  _region_i := !_region_i + 1;
  name

(* contain : string -> Type.t -> bool *)
let rec contain a t =
  match t with
  | TChan(l, [], r) ->
     false
  | TChan(l, t :: ts, r) ->
     if contain a t then
       true
     else
       contain a (TChan(l, ts, r))
  | TVar(a') when a' = a ->
     true
  | _ ->
     false

(* sbst : (string * Type.t) list -> Type.t -> Type.t *)
let rec sbst sigma t =
  match t with
  | TUnit -> TUnit
  | TBool -> TBool
  | TInt -> TInt
  | TChan(l, ts, r) -> TChan(l, List.map (sbst sigma) ts, r)
  | TVar(a) ->
     begin
       try
	 let t = snd @@ List.find (fun (sym, t) -> sym = a) sigma in
	 t
       with
       | Not_found -> TVar(a)
     end
