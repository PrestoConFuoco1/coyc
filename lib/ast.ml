open Sexplib.Conv

type identifier = [ `Identifier of string ]
  [@@deriving sexp]

type unary_pos = [ `Pre | `Post ]
  [@@deriving sexp]

type inc_dec =
  [ `Decrement
  | `Increment
  ]
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
  | `IncDec of unary_pos * inc_dec * identifier
  | `Binary of binary_operator * expression * expression
  | `Assign of identifier * expression
  | `Var of identifier
  | `Ternary of (expression * expression * expression)
  ]
  [@@deriving sexp]

type statement =
  [ `Return of expression
  | `Expression of expression
  | `IfElse of (expression * statement * statement option)
  ]
  [@@deriving sexp]

type declaration = [ `Declare of (identifier * expression option) ]
  [@@deriving sexp]

type block_item =
  [ statement
  | declaration
  ]
  [@@deriving sexp]

type function_definition =
  { name : [`Identifier of string]
  ; body : block_item list
  }
  [@@deriving sexp]

type program =
  { funcs : function_definition
  }
  [@@deriving sexp]
