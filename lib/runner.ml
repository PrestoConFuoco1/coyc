[@@@part "0"] ;;
open Core
open Printf
open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

[@@@ocaml.warning "-8-27"]
let print_token tok = printf "%s" (match tok with
  | Parser.IDENTIFIER i -> "ident")

let parse_with_error lexbuf =
  try Parser.prog (fun l -> let t = Lexer.read l in (); t) lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

[@@@part "1"] ;;

let translate_expression : Ast.expression -> Asm.instruction list = function
  | `Constant i ->
      Asm.[
        Mov (Immediate i, Register)
      ]

let translate_statement = function
  | Ast.Return expr -> translate_expression expr

let translate_function ast_func : Asm.function_definition =
  let body = Ast.(translate_statement ast_func.body @ [Ret]) in
  {Asm.name = ast_func.name; body}

let translate_program ast_prog =
  {Asm.funcs = translate_function Ast.(ast_prog.funcs)}

[@@@part "2"] ;;

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
(*     print_s ([%sexp (value : Ast.program)]); *)
    let asm = translate_program value in
(*     print_s ([%sexp (asm : Asm.program)]); *)
    let rendered_asm = Asm.render_program asm in
(*     printf "%s" rendered_asm; *)
    Out_channel.write_all "out.s" ~data:rendered_asm;
    parse_and_print lexbuf
  | None -> ()

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx

let runner () =
  Command.basic_spec ~summary:"Parse and display JSON"
    Command.Spec.(empty +> anon ("filename" %: string))
    loop
  |> Command_unix.run
