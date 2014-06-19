%{
open Syntax
let addtyp x = (x, Type.gentyp ())
open Utils
%}

%token <int> INT
%token <bool> BOOL
%token NOT LESS_GREATER LESS GREATER GREATER_EQUAL LESS_EQUAL
%token IF THEN ELSE
%token <string> IDENT
%token LET EQUAL IN 
%token REC
%token MINUS
%token PLUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token PRINT
%token EOF
%right prec_let
%right SEMICOLON
%right prec_if
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS
%left prec_app
%left DOT

%type <Syntax.t> exp
%start exp

%%

simple_exp:
| LPAREN exp RPAREN
    { $2 }
| LPAREN RPAREN
    { Unit }
| INT
    { Int($1) }
| BOOL
    { Bool($1) }
| IDENT
    { Var($1) }
exp:
| simple_exp
    { $1 }
| exp PLUS exp
    { Add($1, $3) }
| exp MINUS exp
    { Sub($1, $3) }
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { Let(addtyp $2, $4, $6) }
| LET REC fundef IN exp
    %prec prec_let
    { LetRec($3, $5) }
| exp actual_args
    %prec prec_app
    { App($1, $2) }
| exp SEMICOLON exp
    { Let((genid(".."), Type.Unit), $1, $3) }
| PRINT simple_exp
    %prec prec_app
    { Print($2) }
| NOT exp
    %prec prec_app
    { Not($2) }
| exp EQUAL exp
    { Eq($1, $3) }
| exp LESS_GREATER exp
    { Not(Eq($1, $3)) }
| exp LESS exp
    { Not(LE($3, $1)) }
| exp GREATER exp
    { Not(LE($1, $3)) }
| exp LESS_EQUAL exp
    { LE($1, $3) }
| exp GREATER_EQUAL exp
    { LE($3, $1) }
| IF exp THEN exp ELSE exp
    %prec prec_if
    { If($2, $4, $6) }
| error
    { failwith
      (Printf.sprintf "parse error near characters %d-%d"
        (Parsing.symbol_start ())
        (Parsing.symbol_end ())) }

fundef:
| IDENT formal_args EQUAL exp
    { { name = addtyp $1; args = $2; body = $4 } }

formal_args:
| IDENT formal_args
    { addtyp $1 :: $2 }
| IDENT
    { [addtyp $1] }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }
