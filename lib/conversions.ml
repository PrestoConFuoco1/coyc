open Base
open Stdio

let get_identifier (`Identifier id) : string = id

module State = Monads.Std.Monad.State

let rec map_from_left lst ~f : ('b list, 'c) State.t =
    match lst with
    | [] -> State.return []
    | x :: xs ->
        let open State.Let_syntax in
        let%bind x' = f x in
        let%bind xs' = map_from_left xs ~f in
        State.return (x' :: xs')


let label_counter = ref 0

let get_new_label_idx () =
  let res = !label_counter in
  Int.incr label_counter;
  res

let get_new_block_idx = get_new_label_idx

let mk_fresh_label label () = 
  let n = get_new_label_idx () in
  `Label (`Identifier (label ^ Int.to_string n))

let mk_fresh_false_label = mk_fresh_label "false_label"
let mk_fresh_true_label = mk_fresh_label "true_label"
let mk_fresh_end_label = mk_fresh_label "end"
let mk_fresh_ifelse_false_label = mk_fresh_label "ifelse_false"

module Ast_to_tacky = struct

(* type accum = (string, Base.String.comparator_witness) Base.Set.t *)
type accum_var_map = (string, Tacky.user_var, Base.String.comparator_witness) Base.Map.t
type accum =
  { var_map : accum_var_map
  ; break_label : Tacky.label option
  ; continue_label : Tacky.label option
  }
let empty_accum =
  let var_map = Map.empty (module String) in
  let break_label = None in
  let continue_label = None in
  {var_map; break_label; continue_label}

type 'a monad = ('a, accum) State.t

let var_name_list = ["camel"; "tiger"; "koala"; "rat"; "goat"]

let mk_fresh_var () : string =
  let rand_int = get_new_label_idx () in
  let var_name = List.random_element_exn var_name_list in
  var_name ^ Int.to_string rand_int

let rec translate_and_expression expr1 expr2 : (Tacky.instruction list * Tacky.value) monad =
  let open State.Let_syntax in
  let%bind (instrs1, val1) = translate_expression expr1 in
  let%bind (instrs2, val2) = translate_expression expr2 in
  let false_label = mk_fresh_false_label () in
  let end_label = mk_fresh_end_label () in
  let result_var = `Var (`Identifier (mk_fresh_var ())) in
  State.return (List.concat
        [ instrs1
        ; [`JumpIfZero (val1, false_label)]
        ; instrs2
        ; [`JumpIfZero (val2, false_label)
        ; `Copy (`Constant 1, result_var)
        ; `Jump end_label
        ; false_label
        ; `Copy (`Constant 0, result_var)
        ; end_label]
        ], result_var)

and translate_or_expression expr1 expr2 : (Tacky.instruction list * Tacky.value) monad =
  let open State.Let_syntax in
  let%bind (instrs1, val1) = translate_expression expr1 in
  let%bind (instrs2, val2) = translate_expression expr2 in
  let true_label = mk_fresh_true_label () in
  let end_label = mk_fresh_end_label () in
  let result_var = `Var (`Identifier (mk_fresh_var ())) in
  State.return (List.concat
        [ instrs1
        ; [`JumpIfNotZero (val1, true_label)]
        ; instrs2
        ; [`JumpIfNotZero (val2, true_label)
        ; `Copy (`Constant 0, result_var)
        ; `Jump end_label
        ; true_label
        ; `Copy (`Constant 1, result_var)
        ; end_label]
        ], result_var)

and check_variable_exists (`Identifier var_name) : bool monad =
  let open State.Let_syntax in
  let%bind var_map = State.gets (fun acc -> acc.var_map) in
  State.return (Map.mem var_map var_name)

and unknown_user_var_msg (`Identifier id) = "Variable doesn't exist: " ^ id

and report_if_variable_doesn't_exist var =
  let open State.Let_syntax in
  let%bind exists = check_variable_exists var in
  if not exists then failwith (unknown_user_var_msg var);
  State.return ()

and get_user_var (`Identifier var_name) : Tacky.user_var option monad =
  let open State.Let_syntax in
  let%bind var_map = State.gets (fun acc -> acc.var_map) in
  State.return (Map.find var_map var_name)

and get_user_var_exn var_identifier : Tacky.user_var monad =
  let open State.Let_syntax in
  match%bind get_user_var var_identifier with
  | None -> failwith (unknown_user_var_msg var_identifier)
  | Some var -> State.return var

and get_user_var_value var_identifier : Tacky. value monad =
  State.map (get_user_var_exn var_identifier) ~f:(fun x -> (x :> Tacky.value))

and translate_expression (ast_expr : Ast.expression) : (Tacky.instruction list * Tacky.value) monad =
  let open State.Let_syntax in
  match ast_expr with
  | `Constant const -> State.return ([], `Constant const)
  | `Unary (unop, expr) ->
      let%bind (instrs, val_) = translate_expression expr in
      let new_var = `Var (`Identifier (mk_fresh_var ())) in
      let assign = `Unary (unop, val_, new_var) in
      State.return (instrs @ [assign], new_var)
  | `IncDec (pos, incdec, var) ->
      let%bind user_var = get_user_var_value var in
(*
      let%bind () = report_if_variable_doesn't_exist var in
      let user_var = `UserVar var in
*)
      let new_var = `Var (`Identifier (mk_fresh_var ())) in
      State.return ([`IncDec (pos, incdec, user_var, new_var)], new_var)
  | `Binary (binop, expr1, expr2) ->
      (match binop with
      | `And -> translate_and_expression expr1 expr2
      | `Or -> translate_or_expression expr1 expr2
      | (`Equal | `NotEqual | `LesserThan | `LesserOrEqual |
         `GreaterThan | `GreaterOrEqual |
         `Add | `Subtract | `Multiply | `Divide | `Mod as binop_) ->
        let%bind (instrs1, val1) = translate_expression expr1 in
        let%bind (instrs2, val2) = translate_expression expr2 in
        let new_var = `Var (`Identifier (mk_fresh_var ())) in
        let assign = `Binary (binop_, val1, val2, new_var) in
        State.return (List.concat [instrs1; instrs2; [assign]], new_var))
  | `Assign (var, expr) ->
(*       let%bind () = report_if_variable_doesn't_exist var in *)
      let%bind user_var = get_user_var_value var in
      let%bind (instrs, val_) = translate_expression expr in
(*       let user_var = `UserVar var in *)
      State.return (instrs @ [`Copy (val_, user_var)], user_var)
  | `Var var ->
      let%bind user_var = get_user_var_value var in
      State.return ([], user_var)
  | `Ternary (cond_expr, true_expr, false_expr) ->
      let%bind (cond_instrs, cond_val) = translate_expression cond_expr in
      let new_var = `Var (`Identifier (mk_fresh_var ())) in
      let%bind (t_instrs, true_val) = translate_expression true_expr in
      let%bind (f_instrs, false_val) = translate_expression false_expr in
      let false_label = mk_fresh_ifelse_false_label () in
      let end_label = mk_fresh_end_label () in
      State.return @@
      (List.concat
      [ cond_instrs
      ; [`JumpIfZero (cond_val, false_label)]
      ; t_instrs
      ; [`Copy (true_val, new_var)]
      ; [`Jump end_label]
      ; [false_label]
      ; f_instrs
      ; [`Copy (false_val, new_var)]
      ; [end_label]
      ]
      , new_var)

let report_variable_already_exists (`Identifier id) = "Variable already exists: " ^ id

let insert_new_user_var
      (block_index : Tacky.block_index) (`Identifier var_name as var_identifier) : Tacky.user_var monad =
  let open State.Let_syntax in
  let%bind opt_old_var = get_user_var var_identifier in
  let is_defined_in_current_block =
    match opt_old_var with
    | None -> false
    | Some (`UserVar (_, var_block_index)) -> block_index = var_block_index in
  if is_defined_in_current_block
  then failwith (report_variable_already_exists var_identifier)
  else
    let new_var = `UserVar (var_identifier, block_index) in
    let%bind () = State.update (fun acc -> {acc with var_map = Map.set acc.var_map ~key:var_name ~data:new_var}) in
    State.return new_var

let with_local_changed_state (change_locally : accum -> accum) action : 'a monad =
  let open State.Let_syntax in
  let%bind saved_state = State.get () in
  let changed_state = change_locally saved_state in
  let%bind () = State.put changed_state in
  let%bind res = action in
  let%bind () = State.put saved_state in
  State.return res

let with_break_label (label : Tacky.label) action : 'a monad =
  with_local_changed_state (fun acc -> {acc with break_label = Some label}) action

let with_continue_label (label : Tacky.label) action : 'a monad =
  with_local_changed_state (fun acc -> {acc with continue_label = Some label}) action

let with_new_block (action : Tacky.block_index -> 'a monad) : 'a monad =
  let open State.Let_syntax in
  let new_block_index = get_new_block_idx () in
  let%bind saved_state = State.get () in
  let%bind res = action new_block_index in
  (* Changes made inside compound block are discarded *)
  let%bind () = State.put saved_state in
  State.return res

let rec translate_statement (ast_stmt : Ast.statement) : (Tacky.instruction list) monad =
  let open State.Let_syntax in
  match ast_stmt with
  | `Expression opt_expr ->
      (match opt_expr with
      | None -> State.return []
      | Some expr ->
        let%bind (instrs, _) = translate_expression expr in
        State.return instrs)
  | `IfElse (cond_expr, true_stmt, opt_false_stmt) ->
      let%bind (cond_instrs, cond_val) = translate_expression cond_expr in
      let%bind true_instrs = translate_statement true_stmt in
      let end_label = mk_fresh_end_label () in
      (match opt_false_stmt with
      | None -> State.return @@ List.concat
        [ cond_instrs
        ; [`JumpIfZero (cond_val, end_label)]
        ; true_instrs
        ; [end_label]
        ]
      | Some false_stmt ->
        let%map false_instrs = translate_statement false_stmt in
        let false_label = mk_fresh_ifelse_false_label () in
        List.concat
        [ cond_instrs
        ; [`JumpIfZero (cond_val, false_label)]
        ; true_instrs
        ; [`Jump end_label]
        ; [false_label]
        ; false_instrs
        ; [end_label]
        ])
  | `Compound block_item_list ->
      with_new_block (fun new_block_index ->
        let%bind res = map_from_left block_item_list ~f:(translate_block_item new_block_index) in
        State.return (List.concat res))
  | `Return expr ->
      let%bind (instrs, final_val) = translate_expression expr in
      State.return (instrs @ [`Return final_val])
  | `ForLoop (init_clause, cond_expr, opt_post_expr, body_stmt) ->
      with_new_block (fun new_block_index ->
      (* For loop is in fact a compound block *)
      let%bind init_instrs =
        match init_clause with
        | None -> State.return []
        | Expr init_expr ->
            let%bind (instrs, _result_val) = translate_expression init_expr in
            State.return instrs
        | Decl init_declaration -> translate_declaration new_block_index init_declaration in
      let cond_label = mk_fresh_label "for_cond" () in
      let%bind (cond_instrs, cond_val) = translate_expression cond_expr in
      let end_label = mk_fresh_end_label () in
      let body_end_label = mk_fresh_label "for_body_end" () in
      let%bind body_instrs =
        with_break_label end_label
        (with_continue_label body_end_label
        (translate_statement body_stmt)) in
      let%bind post_instrs : Tacky.instruction list monad =
        match opt_post_expr with
        | None -> State.return []
        | Some post_expr -> State.map ~f:fst (translate_expression post_expr) in
      State.return @@ List.concat
        [ init_instrs
        ; [cond_label]
        ; cond_instrs
        ; [`JumpIfZero (cond_val, end_label)]
        ; body_instrs
        ; [body_end_label]
        ; post_instrs
        ; [`Jump cond_label]
        ; [end_label]
        ])
  | `WhileLoop (cond_expr, body_stmt) ->
      let%bind (cond_instrs, cond_val) = translate_expression cond_expr in
      let cond_label = mk_fresh_label "while_cond" () in
      let end_label = mk_fresh_end_label () in
      let body_end_label = mk_fresh_label "while_body_end" () in
      let%bind body_instrs =
        with_break_label end_label
        (with_continue_label body_end_label
        (translate_statement body_stmt)) in
      State.return @@ List.concat
        [ [cond_label]
        ; cond_instrs
        ; [`JumpIfZero (cond_val, end_label)]
        ; body_instrs
        ; [body_end_label]
        ; [`Jump cond_label]
        ; [end_label]
        ]
  | `DoWhileLoop (body_stmt, post_cond_expr) ->
      let end_label = mk_fresh_end_label () in
      let body_end_label = mk_fresh_label "while_body_end" () in
      let%bind body_instrs =
        with_break_label end_label
        (with_continue_label body_end_label
        (translate_statement body_stmt)) in
      let start_label = mk_fresh_label "dowhile_start" () in
      let%bind (cond_instrs, cond_val) = translate_expression post_cond_expr in
      State.return @@ List.concat
        [ [start_label]
        ; body_instrs
        ; [body_end_label]
        ; cond_instrs
        ; [`JumpIfNotZero (cond_val, start_label)]
        ; [end_label]
        ]
  | `Continue ->
      let%bind opt_continue_label = State.gets (fun acc -> acc.continue_label) in
      State.return (match opt_continue_label with
      | None -> failwith "Unexpected continue label"
      | Some continue_label -> [`Jump continue_label])
  | `Break ->
      let%bind opt_break_label = State.gets (fun acc -> acc.break_label) in
      State.return (match opt_break_label with
      | None -> failwith "Unexpected break label"
      | Some break_label -> [`Jump break_label])

and translate_declaration
      block_index (ast_decl : Ast.declaration) : Tacky.instruction list monad =
  let open State.Let_syntax in
  match ast_decl with
  | `Declare (identifier, opt_expr) ->
      let%bind (init_instrs, init_val) = match opt_expr with
      | None -> State.return ([], `Constant 0)
      | Some expr -> translate_expression expr in
      let%bind new_var = insert_new_user_var block_index identifier in
      State.return (init_instrs @ [`Copy (init_val, (new_var :> Tacky.value))])

and translate_block_item
      block_index (ast_block_item : Ast.block_item) : Tacky.instruction list monad =
  match ast_block_item with
  | Declaration declaration -> translate_declaration block_index declaration
  | Statement stmt -> translate_statement stmt

let translate_function ast_func =
  { Tacky.name = ast_func.Ast.name
  ; Tacky.body =
      let instrs = List.concat (State.eval (map_from_left
        ~f:(translate_block_item (get_new_block_idx ())) ast_func.body) empty_accum) in
      (* TODO: this is very dumb *)
      let has_return = match List.last instrs with | Some (`Return _) -> true | _ -> false in
      if has_return
      then instrs
      else instrs @ [`Return (`Constant 0)]
  }

let translate_program ast_prog =
  {Tacky.funcs = translate_function Ast.(ast_prog.funcs)}

end

let print_tacky_instructions instr_list =
  List.iter instr_list ~f:(fun instr -> print_s ([%sexp_of : Tacky.instruction] instr))

let convert_and_print init_stmt =
  let tacky = State.eval (Ast_to_tacky.translate_statement init_stmt) Ast_to_tacky.empty_accum in
  print_tacky_instructions tacky

(* These test seem to be reproducible because the global pseudo-random generator
 * is always initialised with the same value
 *)
let%expect_test "return constant" =
  convert_and_print @@ `Return (`Constant 2);
  [%expect {| (Return (Constant 2)) |}]

let%expect_test "one unary" =
  convert_and_print @@ `Return (`Unary (`Complement, (`Constant 2)));
  [%expect {|
    (Unary (Complement (Constant 2) (Var (Identifier goat0))))
    (Return (Var (Identifier goat0))) |}]

let%expect_test "one unary" =
  convert_and_print @@ `Return (`Unary (`Complement, (`Unary (`Negate, (`Constant 2)))));
  [%expect {|
    (Unary (Negate (Constant 2) (Var (Identifier goat1))))
    (Unary (Complement (Var (Identifier goat1)) (Var (Identifier koala2))))
    (Return (Var (Identifier koala2))) |}]

module Tacky_to_asm_pseudo = struct

let value_to_operand (tacky_value : Tacky.value) =
  match tacky_value with
  | `Constant const -> Asm.Immediate const
  (* separate namespaces for two types of pseudo registers *)
  | `Var (`Identifier identifier) -> Asm.Pseudo (`Identifier ("." ^ identifier))
  | `UserVar (`Identifier var_name, block_index) -> Asm.Pseudo (`Identifier (var_name ^ Int.to_string block_index))

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
  | `IncDec (pos, incdec, user_var, out_var) ->
      let out_operand = value_to_operand out_var in
      let user_var_operand = value_to_operand user_var in
      let modification_instr =
        match incdec with
        | `Increment -> Asm.Binary (Add, Immediate 1, user_var_operand)
        | `Decrement -> Asm.Binary (Sub, Immediate 1, user_var_operand) in
      let get_value_instr = Asm.Mov (user_var_operand, out_operand) in
      (match pos with
        | `Pre -> [modification_instr; get_value_instr]
        | `Post -> [get_value_instr; modification_instr])
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
  | `Jump (`Label label) -> [Jmp label]
  | (`JumpIfZero (val_, `Label label) | `JumpIfNotZero (val_, `Label label)) as instr ->
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
  Tacky.
  { Asm.name = tacky_func.name
  ; Asm.body = List.concat_map ~f:translate_instruction tacky_func.body
  }

let translate_program (tacky_prog : Tacky.program) =
  { Asm.funcs = translate_function tacky_prog.funcs }

end

module Asm_pseudo_to_asm_stack = struct

open Asm

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
  let ast_func = Ast.{name = `Identifier "test"; body = [(Statement init_func)]} in
  let asm_stack_func =
    Asm_pseudo_to_asm_stack.translate_function @@
      Tacky_to_asm_pseudo.translate_function @@
      Ast_to_tacky.translate_function ast_func in
  print_asm_instructions Asm.(asm_stack_func.body)

let%expect_test "return constant" =
  convert_and_print_total_func @@ `Return (`Constant 2);
  [%expect {|
    (Mov (Immediate 2) (Register AX))
    Ret |}]

let%expect_test "one unary" =
  convert_and_print_total_func @@ `Return (`Unary (`Complement, (`Constant 2)));
  [%expect {|
    (AllocateStack 4)
    (Mov (Immediate 2) (Stack -4))
    (Unary Not (Stack -4))
    (Mov (Stack -4) (Register AX))
    Ret |}]

let%expect_test "one unary" =
  convert_and_print_total_func @@ `Return (`Unary (`Complement, (`Unary (`Negate, (`Constant 2)))));
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
