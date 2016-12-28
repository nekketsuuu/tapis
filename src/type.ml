type level =
  | LInt of int
  | LVar of string
[@@deriving show]

let min_level = 1

type region = string
[@@deriving show]

type t =
  | TChan of level option * t list * region option
  | TUnit
  | TBool
  | TInt
  | TVar  of string
[@@deriving show]

let _type_i = ref 0
let gensym_type () =
  let name = "'a" ^ string_of_int (!_type_i) in
  _type_i := !_type_i + 1;
  name
let rec gensym_type_list n =
  if n <= 0 then []
  else gensym_type :: gensym_type_list (n-1)

let _region_i = ref 0
let gensym_region () =
  let name = "'r" ^ string_of_int (!_region_i) in
  _region_i := !_region_i + 1;
  name

let _level_i = ref 0
let gensym_level () =
  let name = "'l" ^ string_of_int (!_level_i) in
  _level_i := !_level_i + 1;
  name

(* is_chan : t -> bool *)
let is_chan ty =
  match ty with
  | TChan(_) -> true
  | _ -> false

(* contain : string -> t -> bool *)
let rec contain a t =
  match t with
  | TChan(_, [], _) ->
     false
  | TChan(lo, ty :: tys, ro) ->
     if contain a ty then
       true
     else
       contain a (TChan(lo, tys, ro))
  | TVar(a') when a' = a ->
     true
  | _ ->
     false

(* sbst : t sbst -> t -> t *)
let rec sbst sigma t =
  match t with
  | TUnit -> TUnit
  | TBool -> TBool
  | TInt -> TInt
  | TChan(lo, tys, ro) -> TChan(lo, List.map (sbst sigma) tys, ro)
  | TVar(a) ->
     begin
       try
	 List.assoc a sigma
       with
       | Not_found -> TVar(a)
     end

(*
 * pretty print
 *)

open Format

let print_level_option lo =
  match lo with
  | Some(l) ->
     (match l with
      | LInt(i) -> print_string @@ "(" ^ string_of_int i ^ ")"
      | LVar(x) -> print_string @@ "(" ^ x ^ ")")
  | None    -> ()
let print_region_option ro =
  match ro with
  | Some(r) -> print_string r
  | None    -> print_string "?"

let rec print_t ty =
  open_box 1;
  begin 
    match ty with
    | TChan(lo, tys, ro) ->
       print_string "#";
       print_level_option lo;
       print_string "[";
       open_hovbox 0;
       print_t_list tys;
       print_string ";";
       print_space ();
       print_region_option ro;
       close_box ();
       print_string "]";
    | TUnit ->
       print_string "unit"
    | TBool ->
       print_string "bool"
    | TInt ->
       print_string "int"
    | TVar(a) ->
       print_string a
  end;
  close_box ()
and print_t_list tys =
  match tys with
  | [] -> ()
  | ty :: tys ->
     print_t ty;
     List.iter
       (fun ty -> print_string ",";
		  print_space ();
		  print_t ty)
       tys
