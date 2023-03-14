open Base
open Sexplib.Conv
open Printf

type operand =
  | Immediate of int
  | Register
  [@@deriving sexp]

type instruction =
  | Mov of (operand * operand)
  | Ret
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

(****************************************************)

let render_operand = function
  | Immediate i -> sprintf "$%d" i
  | Register -> sprintf "%%%s" "eax"

let render_instruction = function
  | Mov (o1, o2) -> sprintf "\tmovl\t%s, %s" (render_operand o1) (render_operand o2)
  | Ret -> "\tret"

let render_function p =
  let `Identifier fname = p.name in
  let rend_instr = List.map p.body ~f:render_instruction in
  sprintf "%s:" fname :: rend_instr

let render_program (p : program) =
  let main_glob = "\t.globl\tmain" in
  CCString.unlines (main_glob :: render_function p.funcs)
