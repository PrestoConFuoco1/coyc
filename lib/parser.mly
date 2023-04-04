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

%token EQUAL
%token NOT_EQUAL
%token LESSER
%token GREATER
%token LESSER_OR_EQUAL
%token GREATER_OR_EQUAL

%token NOT
%token AND
%token OR

%left OR
%left AND
%left EQUAL NOT_EQUAL
%left LESSER LESSER_OR_EQUAL GREATER GREATER_OR_EQUAL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV MOD     /* medium precedence */
%nonassoc UMINUS

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
  | const = CONSTANT
    { `Constant const } 
  | u = unop; e = expression %prec UMINUS
    { `Unary (u, e) }
  | PAREN_OPEN; e = expression; PAREN_CLOSE
    { e }
  | e1 = expression; PLUS; e2 = expression { `Binary (`Add, e1, e2) }
  | e1 = expression; MINUS; e2 = expression { `Binary (`Subtract, e1, e2) }
  | e1 = expression; TIMES; e2 = expression { `Binary (`Multiply, e1, e2) }
  | e1 = expression; DIV; e2 = expression { `Binary (`Divide, e1, e2) }
  | e1 = expression; MOD; e2 = expression { `Binary (`Mod, e1, e2) }

  | e1 = expression; LESSER; e2 = expression { `Binary (`LesserThan, e1, e2) }
  | e1 = expression; LESSER_OR_EQUAL; e2 = expression { `Binary (`LesserOrEqual, e1, e2) }
  | e1 = expression; GREATER; e2 = expression { `Binary (`GreaterThan, e1, e2) }
  | e1 = expression; GREATER_OR_EQUAL; e2 = expression { `Binary (`GreaterOrEqual, e1, e2) }

  | e1 = expression; EQUAL; e2 = expression { `Binary (`Equal, e1, e2) }
  | e1 = expression; NOT_EQUAL; e2 = expression { `Binary (`NotEqual, e1, e2) }

  | e1 = expression; AND; e2 = expression { `Binary (`And, e1, e2) }
  | e1 = expression; OR; e2 = expression { `Binary (`Or, e1, e2) }

unop:
  | DECREMENT { failwith "not implemented" }
  | MINUS { `Negate }
  | COMPLEMENT { `Complement }
  | NOT { `Not }
