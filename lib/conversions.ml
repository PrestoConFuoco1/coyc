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
    (Unary (Complement (Constant 2) (Var (Identifier koala574))))
    (Return (Var (Identifier koala574))) |}]

let%expect_test "one unary" =
  convert_and_print @@ Ast.Return (`Unary (`Complement, (`Unary (`Negate, (`Constant 2)))));
  [%expect {|
    (Unary (Negate (Constant 2) (Var (Identifier koala574))))
    (Unary (Complement (Var (Identifier koala574)) (Var (Identifier camel368))))
    (Return (Var (Identifier camel368))) |}]

module Tacky_to_asm_pseudo = struct

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

(* let undefined = failwith "" *)

module Asm_pseudo_to_asm_stack = struct

open Asm

module State = Monads.Std.Monad.State

type accum = int * (string, int, Base.String.comparator_witness) Base.Map.t

let replace_operand (asm_pseudo_opd : operand) : (operand, accum) State.t =
  match asm_pseudo_opd with
  | Immediate _ | Register _ -> State.return asm_pseudo_opd
  | Pseudo (`Identifier identifier) ->
      let open State.Let_syntax in
      let%bind (current_offset, offset_map) as old_accum = State.get () in

      let (new_op, new_accum) =
        let opt_existing_offset = Map.find offset_map identifier in
        match opt_existing_offset with
        | None ->
            let hald_of_word = 4 in (* currently we handle only ints, which are 32bit *)
            let new_current_offset = current_offset + hald_of_word in
            let new_offset_map_or_duplicate =
              Map.add offset_map ~key:identifier ~data:new_current_offset in
            let new_offset_map =
              match new_offset_map_or_duplicate with
              | `Ok new_offset_map -> new_offset_map
              | `Duplicate -> failwith "Internal error: failed to insert new pseudo register"
              in
            (Stack (- new_current_offset), (new_current_offset, new_offset_map))
        | Some existing_offset -> (Stack (- existing_offset), old_accum) in
      let%bind () = State.put new_accum in
      State.return new_op
  | Stack _ ->
      failwith "Internal error: there shouldn't be any stack related instructions before this step"

let replace_step instr =
  match instr with
  | Mov (op1, op2) ->
      let mov_constr = fun x1 x2 -> Mov (x1, x2) in
      (State.(!$$)) mov_constr (replace_operand op1) (replace_operand op2)
  | Unary (unop, op) -> State.map ~f:(fun x -> Unary (unop, x)) (replace_operand op)
  | AllocateStack _offset ->
      failwith "there shouldn't be any stack related instructions before this step"
  | Ret -> State.return Ret

let fix_instruction asm_pseudo_instr : Asm.instruction list =
  match asm_pseudo_instr with
(*   | Mov (Stack _ as op1, Stack _ as op2) -> [Mov (op1, Register R10); Mov (Register R10, op2)] *)
  | Mov ((Stack _ as op1), (Stack _ as op2)) -> [Mov (op1, Register R10); Mov (Register R10, op2)]
  | Mov _ | Unary _ | AllocateStack _ | Ret -> List.return asm_pseudo_instr

let translate_function asm_pseudo_func =
  let init = ((0, Map.empty (module String)) : accum) in
  let rec map_from_left lst ~f : ('b list, 'c) State.t =
    match lst with
    | [] -> State.return []
    | x :: xs ->
        let open State.Let_syntax in
        let%bind x' = f x in
        let%bind xs' = map_from_left xs ~f in
        State.return (x' :: xs') in
  let (new_instructions, (final_offset, _)) =
    State.run (map_from_left asm_pseudo_func.body ~f:replace_step) init in
  let new_instructions_fixed = List.concat_map new_instructions ~f:fix_instruction in
  let add_allocate_stack =
    if final_offset = 0
    then Fn.id
    else fun instrs -> AllocateStack final_offset :: instrs in
  { asm_pseudo_func with body = add_allocate_stack new_instructions_fixed }

let translate_program (tacky_prog : Asm.program) =
  { Asm.funcs = translate_function tacky_prog.funcs }

end

let print_asm_instructions instr_list =
  List.iter instr_list ~f:(fun instr -> print_s ([%sexp_of : Asm.instruction] instr))

let convert_and_print_total_func (init_func : Ast.statement) =
  let ast_func = Ast.{name = `Identifier "test"; body = init_func} in
  let asm_stack_func =
    Asm_pseudo_to_asm_stack.translate_function @@
      Tacky_to_asm_pseudo.translate_function @@
      Ast_to_tacky.translate_function ast_func in
  print_asm_instructions Asm.(asm_stack_func.body)

let%expect_test "return constant" =
  convert_and_print_total_func @@ Ast.Return (`Constant 2);
  [%expect {|
    (Mov (Immediate 2) (Register AX))
    Ret |}]

let%expect_test "one unary" =
  convert_and_print_total_func @@ Ast.Return (`Unary (`Complement, (`Constant 2)));
  [%expect {|
    (AllocateStack 4)
    (Mov (Immediate 2) (Stack -4))
    (Unary Not (Stack -4))
    (Mov (Stack -4) (Register AX))
    Ret |}]

let%expect_test "one unary" =
  convert_and_print_total_func @@ Ast.Return (`Unary (`Complement, (`Unary (`Negate, (`Constant 2)))));
  [%expect {|
    (AllocateStack 8)
    (Mov (Immediate 2) (Stack -4))
    (Unary Neg (Stack -4))
    (Mov (Stack -4) (Register R10))
    (Mov (Register R10) (Stack -8))
    (Unary Not (Stack -8))
    (Mov (Stack -8) (Register AX))
    Ret |}]
