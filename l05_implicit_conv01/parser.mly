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
%token COLON

%right prec_let
%right SEMICOLON
%left EQUAL COLON
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
    { Int(Type.Int(64), $1) }
| IDENT
    { Var($1) }

exp:
| simple_exp
    { $1 }
| exp PLUS exp
    { Add(Type.gentyp(), $1, $3) }
| exp MINUS exp
    { Sub(Type.gentyp(), $1, $3) }
| exp SEMICOLON exp
    { Let((genid(".."), Type.Unit), $1, $3) }
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { Let(addtyp $2, $4, $6) }
| LET IDENT COLON typ EQUAL exp IN exp
    %prec prec_let
    { Let(($2,$4), $6, $8) }
| PRINT simple_exp
    %prec prec_app
    { Print($2) }
| error
    { failwith
      (Printf.sprintf "parse error near characters %d-%d"
        (Parsing.symbol_start ())
        (Parsing.symbol_end ())) }

typ:
| IDENT {
    match $1 with
    | "int" -> Type.Int(64)
    | "byte" -> Type.Int(8)
    | _ -> assert false
}