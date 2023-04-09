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
  | `FunCall of (identifier * expression list)
  ]
  [@@deriving sexp]

type statement =
  [ `Return of expression
  | `Expression of expression option
  | `IfElse of (expression * statement * statement option)
  | `Compound of block_item list
  | `ForLoop of (for_initializer * expression * expression option * statement)
  | `WhileLoop of (expression * statement)
  | `DoWhileLoop of (statement * expression)
  | `Break    (* These statements are allowed to occur outside loops *)
  | `Continue (* This will be fixed during code generation, not parsing *)
  ]
  [@@deriving sexp]

and for_initializer =
  | None
  | Expr of expression
  | Decl of declaration
  [@@deriving sexp]

and declaration = [ `Declare of (identifier * expression option) ]
  [@@deriving sexp]

and block_item =
  | Statement of statement
  | Declaration of declaration
  [@@deriving sexp]

type type_sig = identifier list
  [@@deriving sexp]

type function_declaration =
  { name : identifier
  ; parameters : type_sig
  }
  [@@deriving sexp]

type function_definition =
  { name : identifier
  ; parameters : type_sig
  ; body : block_item list
  }
  [@@deriving sexp]

type program_unit =
  | FuncDeclaration of function_declaration
  | FuncDefinition of function_definition
  [@@deriving sexp]

type program =
  { program_units : program_unit list
  }
  [@@deriving sexp]
