(* TODO(nekketsuuu): こんなにエラーの種類いらなさそう *)
exception LexErr of string
exception ParseErr of string
exception EvalErr of string
exception TypeErr of string
exception ConvErr of string

exception Nontermination of string
exception TerminationUnknown of string
