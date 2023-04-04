open Base
open Stdio

(* ast to asm *)

(*
module Ast_to_asm = struct

let translate_expression : Ast.expression -> Asm.instruction list = function
  | `Constant i ->
      Asm.[
        Mov (Immediate i, Register (failwith ))
      ]

let translate_statement = function
  | Ast.Return expr -> translate_expression expr

let translate_function ast_func : Asm.function_definition =
  let body = Ast.(translate_statement ast_func.body @ [Ret]) in
  {Asm.name = ast_func.name; body}

let translate_program ast_prog =
  {Asm.funcs = translate_function Ast.(ast_prog.funcs)}

end
*)

(* let undefined = failwith "" *)

let label_counter = ref 0

let get_new_label_idx () =
  let res = !label_counter in
  Int.incr label_counter;
  res

let mk_fresh_false_label () =
  let n = get_new_label_idx () in
  "false_label" ^ Int.to_string n

let mk_fresh_true_label () =
  let n = get_new_label_idx () in
  "true_label" ^ Int.to_string n

let mk_fresh_end_label () =
  let n = get_new_label_idx () in
  "end" ^ Int.to_string n

module Ast_to_tacky = struct

(* use this to make sure that all names in tacky are globally unique *)
(* let global_name_storage =  *)

let var_name_list = ["camel"; "tiger"; "koala"; "rat"; "goat"]

let mk_fresh_var () : string =
  let rand_int = Random.int_incl 100 999 in
  let var_name = List.random_element_exn var_name_list in
  var_name ^ Int.to_string rand_int


let rec translate_expression (ast_expr : Ast.expression) : (Tacky.instruction list * Tacky.value) =
  match ast_expr with
  | `Constant const -> ([], `Constant const)
  | `Unary (unop, expr) ->
      let (instrs, val_) = translate_expression expr in
      let new_var = `Var (`Identifier (mk_fresh_var ())) in
      let assign = `Unary (unop, val_, new_var) in
      (instrs @ [assign], new_var)
  | `Binary (binop, expr1, expr2) ->
      match binop with
      | `And ->
        let (instrs1, val1) = translate_expression expr1 in
        let (instrs2, val2) = translate_expression expr2 in
        let false_label = `Identifier (mk_fresh_false_label ()) in
        let end_label = `Identifier (mk_fresh_end_label ()) in
        let result_var = `Var (`Identifier (mk_fresh_var ())) in
        (List.concat
        [ instrs1
        ; [`JumpIfZero (val1, false_label)]
        ; instrs2
        ; [`JumpIfZero (val2, false_label)
        ; `Copy (`Constant 1, result_var)
        ; `Jump end_label
        ; `Label false_label
        ; `Copy (`Constant 0, result_var)
        ; `Label end_label]
        ], result_var)

      | `Or ->
        let (instrs1, val1) = translate_expression expr1 in
        let (instrs2, val2) = translate_expression expr2 in
        let true_label = `Identifier (mk_fresh_true_label ()) in
        let end_label = `Identifier (mk_fresh_end_label ()) in
        let result_var = `Var (`Identifier (mk_fresh_var ())) in
        (List.concat
        [ instrs1
        ; [`JumpIfNotZero (val1, true_label)]
        ; instrs2
        ; [`JumpIfNotZero (val2, true_label)
        ; `Copy (`Constant 0, result_var)
        ; `Jump end_label
        ; `Label true_label
        ; `Copy (`Constant 1, result_var)
        ; `Label end_label]
        ], result_var)

      | (`Equal | `NotEqual | `LesserThan | `LesserOrEqual |
         `GreaterThan | `GreaterOrEqual |
         `Add | `Subtract | `Multiply | `Divide | `Mod as binop_) ->
        let (instrs1, val1) = translate_expression expr1 in
        let (instrs2, val2) = translate_expression expr2 in
        let new_var = `Var (`Identifier (mk_fresh_var ())) in
        let assign = `Binary (binop_, val1, val2, new_var) in
        (List.concat [instrs1; instrs2; [assign]], new_var)

let translate_statement (ast_stmt : Ast.statement) : Tacky.instruction list =
  let handle_expr expr =
    let (instrs, final_val) = translate_expression expr in
    (instrs :> Tacky.instruction list) @ [ `Return final_val] in

  match ast_stmt with
  | Return (`Constant const) -> [`Return (`Constant const)]
  | Return (`Unary _ as expr) -> handle_expr expr
  | Return (`Binary _ as expr) -> handle_expr expr

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

let translate_instruction (tacky_instr : Tacky.instruction) =
  match tacky_instr with
  | `Return val_ -> [Asm.Mov (value_to_operand val_, Register AX); Ret]
  | `Unary (unop, in_var, out_var) ->
      let out_operand = value_to_operand out_var in
      (* Here out_operand is always a variable *)
      let in_operand = value_to_operand in_var in
      let handle_simple_unop asm_op =
        [Asm.Mov (in_operand, out_operand); Unary (asm_op, out_operand)] in
      (match unop with
      | `Complement -> handle_simple_unop Asm.Not
      | `Negate -> handle_simple_unop Neg
      | `Not ->
          [ Asm.Cmp (Immediate 0, in_operand)
          ; Mov (Immediate 0, out_operand)
          ; SetCC (E, out_operand)
          ])

  | `Binary (binop, in1_var, in2_var, out_var) ->
      let in1_op = value_to_operand in1_var in
      let in2_op = value_to_operand in2_var in
      let out_op = value_to_operand out_var in
      let handle_simple_operation (asm_op : Asm.binary_operator) =
        [ Asm.Mov (in1_op, out_op)
        ; Asm.Binary (asm_op, in2_op, out_op)
        ] in

      (match binop with
      | `Add -> handle_simple_operation Asm.Add
      | `Subtract -> handle_simple_operation Asm.Sub
      | `Multiply -> handle_simple_operation Asm.IMul
      | (`Divide | `Mod) as binop_ ->
          let resulting_register =
            match binop_ with
            | `Divide -> Asm.AX
            | `Mod -> DX in
          [ Asm.Mov (in1_op, Register AX)
          ; Cdq
          ; IDiv in2_op
          ; Mov (Register resulting_register, out_op)
          ]
      | (`LesserThan | `LesserOrEqual | `GreaterThan | `GreaterOrEqual | `Equal | `NotEqual) as binop_ ->
          let condition =
            match binop_ with
            | `LesserThan -> Asm.L
            | `LesserOrEqual -> LE
            | `GreaterThan -> G
            | `GreaterOrEqual -> GE
            | `Equal -> E
            | `NotEqual -> NE in
          [ Cmp (in2_op, in1_op)
          ; Mov (Immediate 0, out_op) (* mov doesn't change rflags*)
          ; SetCC (condition, out_op) (* Later we'll replace 32-registers with 8 ones *)
          ])
  | `Copy (in_var, out_var) ->
      let in_op = value_to_operand in_var in
      let out_op = value_to_operand out_var in
      [Mov (in_op, out_op)]
  | `Jump label -> [Jmp label]
  | (`JumpIfZero (val_, label) | `JumpIfNotZero (val_, label)) as instr ->
      let condition =
        match instr with
        | `JumpIfZero _ -> Asm.E
        | `JumpIfNotZero _ -> NE in
      let val_op = value_to_operand val_ in
      [ Asm.Cmp (Asm.Immediate 0, val_op)
      ; JmpCC (condition, label)
      ]
  | `Label label -> [Asm.Label label]


let translate_function tacky_func =
  Tacky.{Asm.name = tacky_func.name; Asm.body = List.concat_map ~f:translate_instruction tacky_func.body}

let translate_program (tacky_prog : Tacky.program) =
  { Asm.funcs = translate_function tacky_prog.funcs }

end

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
  | Unary (unop, op) ->
      State.map ~f:(fun x -> Unary (unop, x)) (replace_operand op)
  | Binary (binop, op1, op2) -> 
      State.(!$$) (fun x y -> Binary (binop, x, y)) (replace_operand op1) (replace_operand op2)
  | Cmp (op1, op2) ->
      let cmp_constr = fun x1 x2 -> Cmp (x1, x2) in
      (State.(!$$)) cmp_constr (replace_operand op1) (replace_operand op2)
  | IDiv op -> State.map ~f:(fun x -> IDiv x) (replace_operand op)
  | SetCC (cond, op) ->
      State.map ~f:(fun x -> SetCC (cond, x)) (replace_operand op)
  | AllocateStack _offset ->
      failwith "there shouldn't be any stack related instructions before this step"
  | (Ret | Cdq | Jmp _ | JmpCC _ | Label _) as const_instr -> State.return const_instr

(* (Cmp (_, _)|Jmp _|JmpCC (_, _)|SetCC (_, _)|Label _) *)

(* We use R10 to fix the first operand and R11 to fix the second one *)
let fix_instruction asm_pseudo_instr : Asm.instruction list =
  match asm_pseudo_instr with
(*   | Mov (Stack _ as op1, Stack _ as op2) -> [Mov (op1, Register R10); Mov (Register R10, op2)] *)
  | Mov ((Stack _ as op1), (Stack _ as op2)) ->
      [ Mov (op1, Register R10)
      ; Mov (Register R10, op2)
      ]
  | Mov _ -> List.return asm_pseudo_instr

  | Binary ((Add | Sub) as binop, (Stack _ as op1), (Stack _ as op2)) ->
      [ Mov (op1, Register R10)
      ; Binary (binop, Register R10, op2)
      ]
  (* For imul, regardless of the src operand, the dest operand can't be memory *)
  | Binary (IMul, op1, (Stack _ as op2)) ->
      [ Mov (op2, Register R11)
      ; Binary (IMul, op1, Register R11)
      ; Mov (Register R11, op2)
      ]
  | Binary _ ->  List.return asm_pseudo_instr

  | IDiv (Immediate _ as op) ->
      [ Mov (op, Register R10)
      ; IDiv (Register R10)
      ]
  | IDiv _ -> List.return asm_pseudo_instr

  | Cmp ((Stack _ as op1), (Stack _ as op2)) ->
      [ Mov (op1, Register R10)
      ; Cmp (Register R10, op2)
      ]
  | Cmp (op1, (Immediate _ as op2)) ->
      [ Mov (op2, Register R11)
      ; Cmp (op1, Register R11)
      ]
  | Cmp _ -> List.return asm_pseudo_instr

  | Unary _ | AllocateStack _ | Ret | Cdq
  | Jmp _ | JmpCC _ | SetCC _ | Label _ -> List.return asm_pseudo_instr

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

let total ast =
  ast |> Ast_to_tacky.translate_program
      |> Tacky_to_asm_pseudo.translate_program
      |> Asm_pseudo_to_asm_stack.translate_program
