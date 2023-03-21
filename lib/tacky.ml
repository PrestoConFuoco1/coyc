open Core

type identifier = [ `Identifier of string ]
  [@@deriving sexp]

type unary_operator =
  [ `Complement 
  | `Negate
  ]
  [@@deriving sexp]

type value =
  [ `Constant of int
  | `Var of identifier
  ]
  [@@deriving sexp]

type assignment = 
  [ `Unary of (unary_operator * value * value) (* first is source, second is dest *)
  ]
  [@@deriving sexp]

type instruction =
  [ `Return of value
  | assignment
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
