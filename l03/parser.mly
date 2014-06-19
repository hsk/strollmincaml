%{
open Syntax
%}

%token <int> INT
%token MINUS
%token PLUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token PRINT
%token EOF

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

exp:
| simple_exp
    { $1 }
| exp PLUS exp
    { Add($1, $3) }
| exp MINUS exp
    { Sub($1, $3) }
| exp SEMICOLON exp
    { Let($1, $3) }
| PRINT simple_exp
    %prec prec_app
    { Print($2) }
| error
    { failwith
      (Printf.sprintf "parse error near characters %d-%d"
        (Parsing.symbol_start ())
        (Parsing.symbol_end ())) }
