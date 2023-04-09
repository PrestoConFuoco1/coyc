open Core

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

type block_index = int
  [@@deriving sexp]

type user_var =
  [ `UserVar of (identifier * block_index)
  | `FunArg of int (* Index of function argument, starting from 0 *)
  ]
  [@@deriving sexp]

type value =
  [ `Constant of int
  | `Var of identifier
  | user_var
  ]
  [@@deriving sexp]

type label =
  [ `Label of identifier ]
  [@@deriving sexp]

type instruction =
  [ `Return of value
  | `Unary of (unary_operator * value * value) (* first is source, second is dest *)
  | `IncDec of (unary_pos * inc_dec * value * value)
  | `Binary of (binary_operator * value * value * value)
  | `Copy of (value * value)
  | `Jump of label
  | `JumpIfZero of (value * label)
  | `JumpIfNotZero of (value * label)
  | label
  | `FunCall of (identifier * value list * value)
  ]
  [@@deriving sexp]

type function_definition =
  { name : [`Identifier of string]
  ; body : instruction list
  }
  [@@deriving sexp]

type program =
  { funcs : function_definition list
  }
  [@@deriving sexp]

(*
 * Assumtions:
 * 1. dst of an unary operation will be Var, not Constant.
 * 2. you always assign value to a variable before you use it.
 * 
 *)
