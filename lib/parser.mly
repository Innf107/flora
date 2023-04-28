%{
open Syntax

let loc (start_pos, end_pos) = 
    Lexing.(Loc.{ 
        file = start_pos.pos_fname;
        start_line = start_pos.pos_lnum + 1;
        start_column = start_pos.pos_cnum - start_pos.pos_bol + 1;
        end_line = end_pos.pos_lnum + 1;
        end_column = end_pos.pos_cnum - end_pos.pos_bol + 1;
    })
%}

%token <string> IDENT
%token <float> NUMBER
%token <string> STRING
%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"
%token LET "let"
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token NIL "nil"
%token EQUALS "="
%token SEMI ";"
%token COMMA ","
%token LAMBDA "λ"
%token ARROW "->"
%token LESS "<"
%token LESSEQUAL "<="
%token DOUBLEEQUAL "=="
%token NOTEQUAL "!="
%token GREATEREQUAL ">="
%token GREATER ">"
%token PLUS "+"
%token MINUS "-"
%token STAR "*"
%token SLASH "/"
%token COLON ":"
%token TILDE "~"
%token AND "&&"
%token OR "||"
%token EOF

%start <statement list> main

%%

sep_trailing(sep, p):
|                               { [] }
| p                             { [$1] }
| p sep sep_trailing(sep, p)    { $1 :: $3 }

main:
| sep_trailing(";", statement) EOF { $1 }

expr:
| expr1 binop0 expr { Binop(loc $loc, $1, $2, $3) }
| expr1 { $1 }

binop0:
| "~" { `Concat }

expr1:
| expr2 binop1 expr1 { Binop(loc $loc, $1, $2, $3) }
| expr2 { $1 }

binop1:
| ":" { `Cons }

expr2:
| expr2 binop2 expr3 { Binop(loc $loc, $1, $2, $3) }
| expr3 { $1 }

binop2:
| "&&" { `And }
| "||" { `Or }

expr3:
| expr3 binop3 expr4 { Binop(loc $loc, $1, $2, $3) }
| expr4 { $1 }

binop3:
| "<"  { `Less }
| "<=" { `LessOrEqual }
| "==" { `Equal }
| "!=" { `NotEqual }
| ">=" { `GreaterOrEqual }
| ">"  { `Greater }

expr4:
| expr4 binop4 expr5 { Binop(loc $loc, $1, $2, $3) }
| expr5 { $1 }

binop4:
| "*" { `Multiply }
| "/" { `Divide }

expr5:
| expr5 binop5 expr_leaf { Binop(loc $loc, $1, $2, $3) }
| expr_leaf { $1 }

binop5:
| "+" { `Add }
| "-" { `Subtract }

expr_leaf:
| IDENT { Var(loc $loc, $1) }
| literal { Literal(loc $loc, $1) }
| expr "(" sep_trailing(",", expr) ")" { App(loc $loc, $1, $3) }
| "λ" IDENT "->" expr { Lambda(loc $loc, [$2], $4) }
| "λ" "(" sep_trailing(",", IDENT) ")" "->" expr { Lambda(loc $loc, $3, $6) }
| "if" expr  "then" expr "else" expr { If(loc $loc, $2, $4, $6) }
| "(" expr ")" { $2 }
| "{" sep_trailing(";", statement) "}" { Sequence($2) }

statement:
| "let" IDENT "=" expr { Let(loc $loc, $2, $4) }
| expr { RunExpr($1) }

literal:
| NUMBER { NumberLit($1) }
| STRING { StringLit($1) }
| "nil"  { NilLit }