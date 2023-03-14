{
open Parser

exception SyntaxError of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let constant = ['0'-'9']+
let int_k = "int"
let return_k = "return"

rule read =
  parse
  | white    { read lexbuf }
  | newline  { Lexing.new_line lexbuf; read lexbuf }
  | constant { CONSTANT (int_of_string (Lexing.lexeme lexbuf)) }
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
  | int_k    { INT_KW }
  | return_k { RETURN_KW }
  | '{'      { BRACE_OPEN }
  | '}'      { BRACE_CLOSE }
  | '('      { PAREN_OPEN }
  | ')'      { PAREN_CLOSE }
  | ';'      { SEMICOLON }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
