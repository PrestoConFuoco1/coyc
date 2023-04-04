open Sexplib.Conv

(*
type ast = [
  | `Constant of int
  | `Identifier of string
  ] [@@deriving sexp]
*)

type identifier = [ `Identifier of string ]
  [@@deriving sexp]

type unary_operator =
  [ `Complement
  | `Negate
  | `Not
  ]
  [@@deriving sexp]

type logical_binary_operator =
  [ `And
  | `Or
  | `Equal
  | `NotEqual
  | `LesserThan
  | `LesserOrEqual
  | `GreaterThan
  | `GreaterOrEqual
  ]
  [@@deriving sexp]

type arith_binary_operator =
  [ `Add
  | `Subtract
  | `Multiply
  | `Divide
  | `Mod
  ]
  [@@deriving sexp]

type binary_operator =
  [ logical_binary_operator
  | arith_binary_operator
  ]
  [@@deriving sexp]

type expression =
  [ `Constant of int
  | `Unary of unary_operator * expression
  | `Binary of binary_operator * expression * expression
  | `Assign of identifier * expression
  | `Var of identifier
  ]
  [@@deriving sexp]

type statement =
  [ `Return of expression
  | `Declare of (identifier * expression option)
  | `Expression of expression
  ]
  [@@deriving sexp]

type function_definition =
  { name : [`Identifier of string]
  ; body : statement list
  }
  [@@deriving sexp]

type program =
  { funcs : function_definition
  }
  [@@deriving sexp]

(****************************************************)

