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
%token PLUS
%token TIMES
%token DIV
%token MOD

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV MOD     /* medium precedence */

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
  | e = factor { e }
  | e1 = expression; PLUS; e2 = expression { `Binary (`Add, e1, e2) }
  | e1 = expression; MINUS; e2 = expression { `Binary (`Subtract, e1, e2) }
  | e1 = expression; TIMES; e2 = expression { `Binary (`Multiply, e1, e2) }
  | e1 = expression; DIV; e2 = expression { `Binary (`Divide, e1, e2) }
  | e1 = expression; MOD; e2 = expression { `Binary (`Mod, e1, e2) }

factor:
  | const = CONSTANT { `Constant const }
  | u = unop; e = expression { `Unary (u, e) }
  | PAREN_OPEN; e = expression; PAREN_CLOSE { e }

unop:
  | DECREMENT { failwith "not implemented" }
  | MINUS { `Negate }
  | COMPLEMENT { `Complement }
