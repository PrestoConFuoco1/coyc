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
let if_k = "if"
let else_k = "else"
let for_k = "for"
let while_k = "while"
let do_k = "do"
let break_k = "break"
let continue_k = "continue"

rule read =
  parse
  | white    { read lexbuf }
  | newline  { Lexing.new_line lexbuf; read lexbuf }
  | int_k    { INT_KW }
  | return_k { RETURN_KW }
  | if_k     { IF_KW }
  | else_k   { ELSE_KW }
  | for_k    { FOR_KW }
  | while_k    { WHILE_KW }
  | do_k     { DO_KW }
  | break_k  { BREAK_KW }
  | continue_k { CONTINUE_KW }
  | constant { CONSTANT (int_of_string (Lexing.lexeme lexbuf)) }
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
  | '{'      { BRACE_OPEN }
  | '}'      { BRACE_CLOSE }
  | '('      { PAREN_OPEN }
  | ')'      { PAREN_CLOSE }
  | ';'      { SEMICOLON }
  | ','      { COMMA }

  | "--"     { DECREMENT }
  | "++"     { INCREMENT }
  | '-'      { MINUS }
  | '+'      { PLUS }
  | '/'      { DIV }
  | '*'      { TIMES }
  | '%'      { MOD }
  | '~'      { COMPLEMENT }
  | '!'      { NOT }

  | "=="     { EQUAL }
  | "!="     { NOT_EQUAL }
  | "<="     { LESSER_OR_EQUAL }
  | ">="     { GREATER_OR_EQUAL }
  | '<'      { LESSER }
  | '>'      { GREATER }
  | "&&"     { AND }
  | "||"     { OR }

  | ':'      { COLON }
  | '?'      { QUESTION }

  | '='      { ASSIGNMENT }

  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
