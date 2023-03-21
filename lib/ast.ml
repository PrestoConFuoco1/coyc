open Sexplib.Conv

(*
type ast = [
  | `Constant of int
  | `Identifier of string
  ] [@@deriving sexp]
*)

type unary_operator =
  [ `Complement
  | `Negate
  ]
  [@@deriving sexp]

type expression =
  [ `Constant of int
  | `Unary of unary_operator * expression
  ]
  [@@deriving sexp]

type statement =
  | Return of expression
  [@@deriving sexp]

type function_definition =
  { name : [`Identifier of string]
  ; body : statement
  }
  [@@deriving sexp]

type program =
  { funcs : function_definition
  }
  [@@deriving sexp]

(****************************************************)

