%{
open Syntax
open Utils
let addtyp x = (x, Type.gentyp ())
%}

%token <int> INT
%token <string> IDENT
%token LET EQUAL IN 
%token MINUS
%token PLUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token PRINT
%token EOF

%right prec_let
%right SEMICOLON
%left EQUAL
%left PLUS MINUS
%left prec_app
%left DOT

%type <Syntax.t> exp
%start exp

%%

simple_exp:
| LPAREN exp RPAREN
    { $2 }
| INT
    { Int($1) }
| IDENT
    { Var($1) }

exp:
| simple_exp
    { $1 }
| exp PLUS exp
    { Add($1, $3) }
| exp MINUS exp
    { Sub($1, $3) }
| exp SEMICOLON exp
    { Let((genid(".."), Type.Unit), $1, $3) }
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { Let(addtyp $2, $4, $6) }
| PRINT simple_exp
    %prec prec_app
    { Print($2) }
| error
    { failwith
      (Printf.sprintf "parse error near characters %d-%d"
        (Parsing.symbol_start ())
        (Parsing.symbol_end ())) }
