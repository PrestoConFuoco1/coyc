open Sexplib.Conv

type ast = [
  | `Constant of int
  | `Identifier of string
  ] [@@deriving sexp]
