exception LexErr of string
exception ParseErr of string
exception EvalErr of string
exception TypeErr of string

let app_name = "piterm"

type name = string
[@@deriving show]

type process =
  | PNil
  | PIn   of name * name list * process
  | POut  of name * expr list * process
  | PRIn  of name * name list * process
  | PPar  of process * process
  | PRes  of name * process
  | PIf   of expr * process * process
and expr =
  | EVar  of name
  | EUnit
  | EBool of bool
  | EInt  of int
  | ENot  of expr
  | EAnd  of expr * expr
  | EOr   of expr * expr
  | ENeg  of expr
  | EAdd  of expr * expr
  | ESub  of expr * expr
  | EMul  of expr * expr
  | EDiv  of expr * expr
  | EEq   of expr * expr
  | ELt   of expr * expr
  | EGt   of expr * expr
  | ELeq  of expr * expr
  | EGeq  of expr * expr
[@@deriving show]
