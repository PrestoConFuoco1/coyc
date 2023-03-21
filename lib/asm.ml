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

type binary_operator = Add | Sub | IMul
  [@@deriving sexp]

type instruction =
  | Mov of operand * operand
  | Binary of binary_operator * operand * operand
  | Unary of unary_operator * operand
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
  | AX -> "eax"
  | R10 -> "r10d"

let binary_instruction =
  sprintf "\t%s\t%s, %s"

let render_operand = function
  | Immediate i -> sprintf "$%d" i
  | Register reg -> sprintf "%%%s" (render_register reg)
  | Pseudo _ -> failwith "Internal error: there shouldn't be pseudo registers at this stage"
  | Stack offset -> sprintf "%d(%%rbp)" offset

let render_unary_operator = function
  | Neg -> "negl"
  | Not -> "notl"

let render_binop = function
  | Add -> "addl"
  | Sub -> "subl"
  | IMul -> "imul"

let function_prologue : string list =
  [ "\tpushq\t%rbp"
  ; binary_instruction "movq" "%rsp" "%rbp"
  ]

let function_epilogue : string list =
  [ binary_instruction "movq" "%rbp" "%rsp"
  ; "\tpopq\t%rbp" ]

let render_instruction = function
  | Mov (o1, o2) ->
      [binary_instruction "movl" (render_operand o1) (render_operand o2)]
  | Unary (unop, o) ->
      [sprintf "\t%s\t%s" (render_unary_operator unop) (render_operand o)]
  | Binary (binop, o1, o2) ->
      [binary_instruction (render_binop binop) (render_operand o1) (render_operand o2)]
  | Ret -> function_epilogue @ ["\tret"]
  | AllocateStack alloc_size ->
      [binary_instruction "subq" (render_operand (Immediate alloc_size)) "%rsp"]

let render_function p =
  let `Identifier fname = p.name in
  let rend_instr = List.concat_map p.body ~f:render_instruction in
  sprintf "%s:" fname :: (function_prologue @ rend_instr)

let render_program (p : program) =
  let main_glob = "\t.globl\tmain" in
  CCString.unlines (main_glob :: render_function p.funcs)
