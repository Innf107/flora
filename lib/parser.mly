%{
open Syntax

let loc (start_pos, end_pos) = Loc.from_positions start_pos end_pos
%}

%token <string> IDENT
%token <float> NUMBER
%token <string> STRING
%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"
%token LBRACKET "["
%token RBRACKET "]"
%token LET "let"
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token NIL "nil"
%token TRUE "true"
%token FALSE "false"
%token PERFORM "perform"
%token HANDLE "handle"
%token MATCH "match"
%token AS "as"
%token EQUALS "="
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
%token DOUBLECOLON "::"
%token TILDE "~"
%token AND "&&"
%token OR "||"
%token PIPE "|"
%token EOF

%start <statement list> main

%%

sep_trailing(sep, p):
|                               { [] }
| p                             { [$1] }
| p sep sep_trailing(sep, p)    { $1 :: $3 }

some(p):
| p                             { [] }
| p some(p)                     { $1 :: $2 }

main:
| statement* EOF { $1 }

expr:
| expr1 binop0 expr { Binop(loc $loc, $1, $2, $3) }
| expr1 { $1 }

binop0:
| "~" { `Concat }

expr1:
| expr2 binop1 expr1 { Binop(loc $loc, $1, $2, $3) }
| expr2 { $1 }

binop1:
| "::" { `Cons }

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
| "+" { `Add }
| "-" { `Subtract }

expr5:
| expr5 binop5 expr_leaf { Binop(loc $loc, $1, $2, $3) }
| expr_leaf { $1 }

binop5:
| "*" { `Multiply }
| "/" { `Divide }

expr_leaf:
| IDENT { Var(loc $loc, $1) }
| literal { Literal(loc $loc, $1) }
| expr_leaf "(" sep_trailing(",", expr) ")" { App(loc $loc, $1, $3) }
| "λ" pattern_leaf "->" expr { Lambda(loc $loc, [$2], $4) }
| "λ" "(" sep_trailing(",", pattern) ")" "->" expr { Lambda(loc $loc, $3, $6) }
| "if" expr "then" block_expr "else" block_expr { If(loc $loc, $2, $4, $6) }
| "perform" IDENT "(" sep_trailing(",", expr) ")" { Perform(loc $loc, $2, $4) }
| "handle" expr "{" sep_trailing(",", handle_branch) "}" { Handle(loc $loc, $2, $4) }
| "match" expr "{" sep_trailing(",", match_branch) "}" { Match(loc $loc, $2, $4) }
| "(" expr ")" { $2 }
| "{" record_or_sequence "}" { $2 }
| "[" sep_trailing(",", expr) "]" { ListLiteral(loc $loc, $2) }

%inline block_expr:
    "{" statement* "}" { Sequence $2 }

record_or_sequence:
|                                                       { RecordLiteral(loc $loc, []) }
| IDENT "=" expr                                        { RecordLiteral(loc $loc, [($1, $3)]) }
| IDENT "=" expr "," sep_trailing(",", record_def)      { RecordLiteral(loc $loc, (($1, $3) :: $5)) } 
| statement statement*                                  { Sequence($1 :: $2) }

handle_branch:
| IDENT "(" sep_trailing(",", pattern) ")" IDENT "->" expr { ($1, $3, $5, $7) }

record_def:
| IDENT "=" expr { ($1,  $3) }

match_branch:
| pattern "->" expr { ($1, $3) }

statement:
| "let" pattern "=" expr { Let(loc $loc, $2, $4) }
| "let" IDENT "(" sep_trailing(",", pattern) ")" "=" expr { LetFun(loc $loc, $2, $4, $7) }
| expr { RunExpr($1) }

pattern:
| pattern1 "|" pattern { OrPat(loc $loc, $1, $3) }
| pattern1             { $1 }

pattern1:
| pattern2 "::" pattern1    { ConsPat(loc $loc, $1, $3) }
| pattern2                  { $1 }

pattern2:
| pattern_leaf "as" IDENT { AsPat(loc $loc, $1, $3) }
| pattern_leaf            { $1 }

pattern_leaf:
| IDENT                                         { VarPat(loc $loc, $1) }
| literal                                       { LiteralPat(loc $loc, $1) }
| "[" sep_trailing(",", pattern) "]"            { ListPat(loc $loc, $2) }
| "{" sep_trailing(",", record_pattern) "}"     { RecordPat(loc $loc, $2) }
| "(" pattern ")"                               { $2 }

record_pattern:
| IDENT "=" pattern { ($1, $3) }

literal:
| NUMBER    { NumberLit($1) }
| STRING    { StringLit($1) }
| "nil"     { NilLit }
| "true"    { BoolLit(true) }
| "false"   { BoolLit(false) }
