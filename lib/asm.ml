open Base
open Sexplib.Conv
open Printf

type identifier = [ `Identifier of string ]
  [@@deriving sexp]

type reg =
  | AX
  | R10
  [@@deriving sexp]

type operand =
  | Immediate of int
  | Register of reg
  | Pseudo of identifier
  | Stack of int (* -4(%rbp) <-> Stack(-4) *)
  [@@deriving sexp]

type unary_operator = Neg | Not
  [@@deriving sexp]

type instruction =
  | Mov of operand * operand
  | Unary of (unary_operator * operand)
  | AllocateStack of int (* subq $n, %rsp *)
  | Ret
  [@@deriving sexp]

type function_definition =
  { name : identifier
  ; body : instruction list
  }
  [@@deriving sexp]

type program =
  { funcs : function_definition
  }
  [@@deriving sexp]

(****************************************************)
let render_register = function
  | AX -> "ax"
  | R10 -> "r10"

let render_operand = function
  | Immediate i -> sprintf "$%d" i
  | Register reg -> sprintf "%%%s" (render_register reg)
  | _ -> failwith ""

let render_unary_operator = function
  | Neg -> "neg"
  | Not -> "not"

let render_instruction = function
  | Mov (o1, o2) -> sprintf "\tmovl\t%s, %s" (render_operand o1) (render_operand o2)
  | Unary (unop, o) -> sprintf "\t%s\t%s" (render_unary_operator unop) (render_operand o)
  | Ret -> "\tret"
  | _ -> failwith ""

let render_function p =
  let `Identifier fname = p.name in
  let rend_instr = List.map p.body ~f:render_instruction in
  sprintf "%s:" fname :: rend_instr

let render_program (p : program) =
  let main_glob = "\t.globl\tmain" in
  CCString.unlines (main_glob :: render_function p.funcs)
