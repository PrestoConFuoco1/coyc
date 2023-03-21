%token <string> IDENTIFIER
%token <int> CONSTANT
%token INT_KW
%token RETURN_KW
%token BRACE_OPEN
%token BRACE_CLOSE
%token PAREN_OPEN
%token PAREN_CLOSE
%token SEMICOLON
%token DECREMENT
%token MINUS
%token COMPLEMENT
%token EOF

%start <Ast.program option> prog

%%

prog: 
  | v = function_definition { Some v }
  | EOF       { None   } ;

function_definition:
  INT_KW; name = IDENTIFIER; PAREN_OPEN; PAREN_CLOSE;
    BRACE_OPEN; body = statement; BRACE_CLOSE
      {
        {Ast.funcs = {Ast.name = `Identifier name; body}}
      };

statement:
  | RETURN_KW; e = expression; SEMICOLON { Ast.Return e };

expression:
  | const = CONSTANT { `Constant const }
  | u = unop; e = expression { `Unary (u, e) }
  | PAREN_OPEN; e = expression; PAREN_CLOSE { e }

unop:
  | DECREMENT { failwith "not implemented" }
  | MINUS { `Negate }
  | COMPLEMENT { `Complement }
