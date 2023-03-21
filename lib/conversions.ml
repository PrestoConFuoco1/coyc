open Base
open Stdio

(* ast to asm *)

module Ast_to_asm = struct

let translate_expression : Ast.expression -> Asm.instruction list = function
  | `Constant i ->
      Asm.[
        Mov (Immediate i, Register (failwith ""))
      ]

let translate_statement = function
  | Ast.Return expr -> translate_expression expr

let translate_function ast_func : Asm.function_definition =
  let body = Ast.(translate_statement ast_func.body @ [Ret]) in
  {Asm.name = ast_func.name; body}

let translate_program ast_prog =
  {Asm.funcs = translate_function Ast.(ast_prog.funcs)}

end

module Ast_to_tacky = struct

(* use this to make sure that all names in tacky are globally unique *)
(* let global_name_storage =  *)

let var_name_list = ["camel"; "tiger"; "koala"; "rat"; "goat"]

let mk_fresh_var () : string =
  let rand_int = Random.int_incl 100 999 in
  let var_name = List.random_element_exn var_name_list in
  var_name ^ Int.to_string rand_int

let rec translate_expression (ast_expr : Ast.expression) : (Tacky.assignment list * Tacky.value) =
  match ast_expr with
  | `Unary (unop, expr) ->
      let (instrs, val_) = translate_expression expr in
      let new_var = `Var (`Identifier (mk_fresh_var ())) in
      let assign = `Unary (unop, val_, new_var) in
      (instrs @ [assign], new_var)
  | `Constant const -> ([], `Constant const)

let translate_statement (ast_stmt : Ast.statement) : Tacky.instruction list =
  match ast_stmt with
  | Return (`Constant const) -> [`Return (`Constant const)]
  | Return (`Unary _ as expr) ->
    let (instrs, final_val) = translate_expression expr in
    (instrs :> Tacky.instruction list) @ [ `Return final_val]

let translate_function ast_func =
  Ast.{Tacky.name = ast_func.name; Tacky.body = translate_statement ast_func.body}

let translate_program ast_prog =
  {Tacky.funcs = translate_function Ast.(ast_prog.funcs)}

end

let print_tacky_instructions instr_list =
  List.iter instr_list ~f:(fun instr -> print_s ([%sexp_of : Tacky.instruction] instr))

let convert_and_print init_stmt =
  let tacky = Ast_to_tacky.translate_statement init_stmt in
  print_tacky_instructions tacky

(* These test seem to be reproducible because the global pseudo-random generator
 * is always initialised with the same value
 *)
let%expect_test "return constant" =
  convert_and_print @@ Ast.Return (`Constant 2);
  [%expect {| (Return (Constant 2)) |}]

let%expect_test "one unary" =
  convert_and_print @@ Ast.Return (`Unary (`Complement, (`Constant 2)));
  [%expect {|
    (Unary (Complement (Constant 2) (Var koala574)))
    (Return (Var koala574)) |}]

let%expect_test "one unary" =
  convert_and_print @@ Ast.Return (`Unary (`Complement, (`Unary (`Negate, (`Constant 2)))));
  [%expect {|
    (Unary (Negate (Constant 2) (Var koala574)))
    (Unary (Complement (Var koala574) (Var camel368)))
    (Return (Var camel368)) |}]

module Tacky_to_asm1 = struct

let value_to_operand (tacky_value : Tacky.value) =
  match tacky_value with
  | `Constant const -> Asm.Immediate const
  | `Var identifier -> Asm.Pseudo identifier

let translate_unop (tacky_unop : Tacky.unary_operator) =
  match tacky_unop with
  | `Complement -> Asm.Not
  | `Negate -> Neg

let translate_instruction (tacky_instr : Tacky.instruction) =
  match tacky_instr with
  | `Return val_ -> [Asm.Mov (value_to_operand val_, Register AX); Ret]
  | `Unary (unop, in_var, out_var) ->
      let out_operand = value_to_operand out_var in
      (* Here out_operand is always a variable *)
      let in_operand = value_to_operand in_var in
      let asm_op = translate_unop unop in
      [Asm.Mov (in_operand, out_operand); Unary (asm_op, out_operand)]

let translate_function tacky_func =
  Tacky.{Asm.name = tacky_func.name; Asm.body = List.concat_map ~f:translate_instruction tacky_func.body}

let translate_program (tacky_prog : Tacky.program) =
  { Asm.funcs = translate_function tacky_prog.funcs }

end
