open Core

type identifier = [ `Identifier of string ]
  [@@deriving sexp]

type unary_operator =
  [ `Complement 
  | `Negate
  | `Not
  ]
  [@@deriving sexp]

type logical_binary_operator =
  [ `Equal
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

type value =
  [ `Constant of int
  | `Var of identifier
  | `UserVar of identifier
  ]
  [@@deriving sexp]

type instruction =
  [ `Return of value
  | `Unary of (unary_operator * value * value) (* first is source, second is dest *)
  | `Binary of (binary_operator * value * value * value)
  | `Copy of (value * value)
  | `Jump of identifier
  | `JumpIfZero of (value * identifier)
  | `JumpIfNotZero of (value * identifier)
  | `Label of identifier
  ]
  [@@deriving sexp]

type function_definition =
  { name : [`Identifier of string]
  ; body : instruction list
  }
  [@@deriving sexp]

type program =
  { funcs : function_definition
  }
  [@@deriving sexp]

(*
 * Assumtions:
 * 1. dst of an unary operation will be Var, not Constant.
 * 2. you always assign value to a variable before you use it.
 * 
 *)
