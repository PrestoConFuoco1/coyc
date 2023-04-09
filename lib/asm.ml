open Base
open Sexplib.Conv
open Printf
(* open Stdio *)

(* let undefined = failwith "" *)

type identifier = [ `Identifier of string ]
  [@@deriving sexp]

type reg =
  | AX
  | DX
  | R10
  | R11
  [@@deriving sexp]

type cond_code = E | NE | G | GE | L | LE
  [@@deriving sexp]

type stack_reg =
  | RSP
  | RBP
  [@@deriving sexp]

type operand =
  | Immediate of int
  | Register of reg
  | Pseudo of identifier
  | FunArg of int
  | Stack of (int * stack_reg) (* -4(%rbp) <-> Stack(-4) *)
  [@@deriving sexp]

type unary_operator = Neg | Not
  [@@deriving sexp]

type binary_operator = Add | Sub | IMul
  [@@deriving sexp]

type instruction =
  | Mov of operand * operand
  | Unary of unary_operator * operand
  | Binary of binary_operator * operand * operand
  | Cmp of operand * operand (* cmp b, a computes a – b *)
  | IDiv of operand
  | Cdq

  | Jmp of identifier
  | JmpCC of cond_code * identifier
  | SetCC of cond_code * operand
  | Label of identifier

  | AllocateStack of int (* subq $n, %rsp *)
  | DeallocateStack of int (* addq $n, %rsp *)
  | Call of identifier
  | Ret
  [@@deriving sexp]

type function_definition =
  { name : identifier
  ; body : instruction list
  }
  [@@deriving sexp]

type program =
  { funcs : function_definition list
  }
  [@@deriving sexp]

(****************************************************)
type operand_size =
  | B1 (* al *)
  | B4 (* eax *)

let reg_sz size (v4, v1) =
  match size with
  | B1 -> v1
  | B4 -> v4

let render_register size = function
  | AX -> reg_sz size ("eax", "al")
  | DX -> reg_sz size ("edx", "dl")
  | R10 -> reg_sz size ("r10d", "r10b")
  | R11 -> reg_sz size ("r11d", "r10b")

let binary_instruction =
  sprintf "\t%s\t%s, %s"

let render_reg_offset offset reg = sprintf "%d(%%%s)" offset reg

let render_stack_reg = function
  | RSP -> "rsp"
  | RBP -> "rbp"

let render_operand size = function
  | Immediate i -> sprintf "$%d" i
  | Register reg -> sprintf "%%%s" (render_register size reg)
  | Pseudo _ ->
      failwith "Internal error: there shouldn't be pseudo registers at this stage"
  | Stack (offset, reg) -> sprintf "%d(%%%s)" offset (render_stack_reg reg)
  | FunArg _ -> failwith "Internal error: there shouldn't be function arguments at this stage"

let render_unary_operator = function
  | Neg -> "negl"
  | Not -> "notl"

let render_binop = function
  | Add -> "addl"
  | Sub -> "subl"
  | IMul -> "imull"

let function_prologue : string list =
  [ "\tpushq\t%rbp"
  ; binary_instruction "movq" "%rsp" "%rbp"
  ]

let function_epilogue : string list =
  [ binary_instruction "movq" "%rbp" "%rsp"
  ; "\tpopq\t%rbp" ]

let render_label (`Identifier label) = "." ^ label
let render_label_colon (`Identifier label) = "." ^ label ^ ":"

let render_condition = function
  | E -> "e"
  | NE -> "ne"
  | G -> "g"
  | GE -> "ge"
  | L -> "l"
  | LE -> "le"

let render_instruction = function
  | Mov (o1, o2) ->
      [binary_instruction "movl" (render_operand B4 o1) (render_operand B4 o2)]
  | Unary (unop, o) ->
      [sprintf "\t%s\t%s" (render_unary_operator unop) (render_operand B4 o)]
  | Binary (binop, o1, o2) ->
      [binary_instruction (render_binop binop) (render_operand B4 o1) (render_operand B4 o2)]
  | IDiv op -> [sprintf "\tidiv\t%s" (render_operand B4 op)]
  | Cdq -> ["\tcdq"]
  | Ret -> function_epilogue @ ["\tret"]
  | AllocateStack alloc_size ->
      [binary_instruction "subq" (render_operand B4 (Immediate alloc_size)) "%rsp"]
  | DeallocateStack alloc_size ->
      [binary_instruction "addq" (render_operand B4 (Immediate alloc_size)) "%rsp"]
  | Label label -> [render_label_colon label]
  | Cmp (o1, o2) -> [binary_instruction "cmpl" (render_operand B4 o1) (render_operand B4 o2)]
  | Jmp label -> [sprintf "\tjmp\t%s" (render_label label)]
  | JmpCC (cond, label) -> [sprintf "\tj%s\t%s" (render_condition cond) (render_label label)]
  | SetCC (cond, op) -> [sprintf "\tset%s\t%s" (render_condition cond) (render_operand B1 op)]
  | Call (`Identifier fun_name) -> [sprintf "\tcall\t%s" fun_name]

let render_function p =
  let `Identifier fname = p.name in
  let rend_instr = List.concat_map p.body ~f:render_instruction in
  sprintf "%s:" fname :: (function_prologue @ rend_instr)

let render_program (p : program) =
(*   print_s [%sexp (p : program)]; *)
  let main_glob = "\t.globl\tmain" in
  CCString.unlines (main_glob :: List.concat_map ~f:render_function p.funcs)
