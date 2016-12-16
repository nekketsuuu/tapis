open Location

exception LexErr of string
exception ParseErr of string
exception EvalErr of string
exception TypeErr of string

let app_name = "piterm"

type name = string * Type.t option
[@@deriving show]
  

type t = process location
and process =
  | PNil
  | PIn   of name * name list * t
  | POut  of name * e list * t
  | PRIn  of name * name list * t
  | PPar  of t * t
  | PRes  of name * t
  | PIf   of e * t * t
and e = expr location
and expr =
  | EVar  of name
  | EUnit
  | EBool of bool
  | EInt  of int
  | ENot  of e
  | EAnd  of e * e
  | EOr   of e * e
  | ENeg  of e
  | EAdd  of e * e
  | ESub  of e * e
  | EMul  of e * e
  | EDiv  of e * e
  | EEq   of e * e
  | ELt   of e * e
  | EGt   of e * e
  | ELeq  of e * e
  | EGeq  of e * e
[@@deriving show]
