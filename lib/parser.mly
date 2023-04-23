%{
open Syntax

let loc (start_pos, end_pos) = 
    Lexing.(Loc.{ 
        file = start_pos.pos_fname;
        start_line = start_pos.pos_lnum;
        start_column = start_pos.pos_cnum - start_pos.pos_bol;
        end_line = end_pos.pos_lnum;
        end_column = end_pos.pos_cnum - end_pos.pos_bol;
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
%token EQUALS "="
%token SEMI ";"
%token COMMA ","
%token LAMBDA "λ"
%token ARROW "->"
%token EOF

%start <expr> main

%%

sep_trailing(sep, p):
|                               { [] }
| p                             { [$1] }
| p sep sep_trailing(sep, p)    { $1 :: $3 }

main:
| expr EOF { $1 }

expr:
| IDENT { Var(loc $loc, $1) }
| literal { Literal(loc $loc, $1) }
| expr "(" sep_trailing(",", expr) ")" { App(loc $loc, $1, $3) }
| "let" IDENT "=" expr ";" expr { Let(loc $loc, $2, $4, $6) }
| "λ" IDENT "->" expr { Lambda(loc $loc, [$2], $4) }
| "λ" "(" sep_trailing(",", IDENT) ")" "->" expr { Lambda(loc $loc, $3, $6) }
| "(" expr ")" { $2 }
| "{" expr "}" { $2 }

literal:
| NUMBER { NumberLit($1) }
| STRING { StringLit($1) }