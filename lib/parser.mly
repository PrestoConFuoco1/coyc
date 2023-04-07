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
%token INCREMENT
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

%token ASSIGNMENT

%token IF_KW
%token ELSE_KW
%token COLON
%token QUESTION

%token FOR_KW
%token WHILE_KW
%token DO_KW
%token BREAK_KW
%token CONTINUE_KW

%right NOELSE ELSE_KW /* shifting wins because of equal precedence */

%nonassoc ASSIGNMENT
%right QUESTION COLON
%left OR
%left AND
%left EQUAL NOT_EQUAL
%left LESSER LESSER_OR_EQUAL GREATER GREATER_OR_EQUAL
%left PLUS MINUS        /* lower precedence */
%left TIMES DIV MOD     /* higher precedence */
%nonassoc UMINUS

%start <Ast.program option> prog

%%

prog: 
  | v = function_definition { Some v }
  | EOF       { None   } ;

function_definition:
  INT_KW; name = IDENTIFIER; PAREN_OPEN; PAREN_CLOSE;
    BRACE_OPEN; body = list(block_item); BRACE_CLOSE
      {
        {Ast.funcs = {Ast.name = `Identifier name; body}}
      };

declaration:
  | INT_KW; name = IDENTIFIER;
      opt_init = option(ASSIGNMENT; e = expression { e }); SEMICOLON
      { `Declare (`Identifier name, opt_init) }

block_item:
  | d = declaration { Declaration d }
  | s = statement { Statement s }

statement:
  | RETURN_KW; e = expression; SEMICOLON { `Return e }
  | e = option(expression); SEMICOLON { `Expression e }

  | IF_KW; PAREN_OPEN; cond_expr = expression; PAREN_CLOSE;
      true_stmt = statement;
      ELSE_KW; false_stmt = statement
      { `IfElse (cond_expr, true_stmt, Some false_stmt) }
  | IF_KW; PAREN_OPEN; cond_expr = expression; PAREN_CLOSE;
      true_stmt = statement %prec NOELSE
      { `IfElse (cond_expr, true_stmt, None) }

  | BRACE_OPEN; items = list(block_item); BRACE_CLOSE { `Compound items }
  | FOR_KW; PAREN_OPEN; init = for_initializer; /* initializer already includes semicolon */
      opt_cond = option(expression); SEMICOLON; opt_post = option(expression); PAREN_CLOSE;
      body = statement;
      { `ForLoop (init, Option.value opt_cond ~default:(`Constant 1), opt_post, body) }
  | WHILE_KW; PAREN_OPEN; cond = expression; PAREN_CLOSE; body = statement
      { `WhileLoop (cond, body) }
  | DO_KW; body = statement; WHILE_KW; PAREN_OPEN; cond = expression; PAREN_CLOSE
      { `DoWhileLoop (body, cond) }

  | CONTINUE_KW { `Continue }
  | BREAK_KW { `Break }

for_initializer:
  | SEMICOLON { Ast.None }
  | e = expression; SEMICOLON { Ast.Expr e }
  | d = declaration { Ast.Decl d } /* declaration already includes terminating semicolon */

expression:
  | const = CONSTANT
    { `Constant const } 
  | u = unop; e = expression %prec UMINUS
    { `Unary (u, e) }
  | var = IDENTIFIER; u = unop_postfix { `IncDec (`Post, u, `Identifier var) }
  | u = unop_postfix; var = IDENTIFIER { `IncDec (`Pre, u, `Identifier var) }

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

  | var = IDENTIFIER; ASSIGNMENT; e = expression { `Assign (`Identifier var, e) }
  | var = IDENTIFIER { `Var (`Identifier var) }

  | cond = expression; QUESTION; true_expr = expression; COLON; false_expr = expression
      { `Ternary (cond, true_expr, false_expr) }

unop:
  | MINUS { `Negate }
  | COMPLEMENT { `Complement }
  | NOT { `Not }

unop_postfix:
  | DECREMENT { `Decrement }
  | INCREMENT { `Increment }
