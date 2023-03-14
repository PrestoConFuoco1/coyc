%token <string> IDENTIFIER
%token <int> CONSTANT
%token INT_KW
%token RETURN_KW
%token BRACE_OPEN
%token BRACE_CLOSE
%token PAREN_OPEN
%token PAREN_CLOSE
%token SEMICOLON
%token EOF

%start <Ast.ast list option> prog

%%

prog: 
  | v = semicolon_list { Some v }
  | EOF       { None   } ;

semicolon_list:
  BRACE_OPEN; obj = separated_list(SEMICOLON, one_token); BRACE_CLOSE    { obj } ;

one_token:
  | s = IDENTIFIER                            { `Identifier s }
  | i = CONSTANT                              { `Constant i }
