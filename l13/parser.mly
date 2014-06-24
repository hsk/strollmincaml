%{
open Syntax
let addtyp x = (x, Type.gentyp ())
%}

%token <int> INT
%token <string> IDENT
%token <bool> BOOL
%token <float> FLOAT
%token ADD_DOT SUB_DOT MUL_DOT DIV_DOT
%token NOT NE EQ LT GT LE GE
%token IF THEN ELSE
%token VALEQ
%token LPAREN RPAREN LBRACE RBRACE
%token LBRACKET RBRACKET ASSIGN ARRAY_CREATE
%token ADD SUB
%token SEMICORON
%token COMMA
%token EOF
%type <Syntax.t> exp
%start exp

%%

exp_prim:
| INT { Int($1) }
| LPAREN elems RPAREN {
  match $2 with
  | [a] -> a
  | a -> Tuple(a)
}
| LBRACE exps RBRACE { Let(addtyp(Utils.genid("..")),Unit, $2) }
| IDENT { Var($1) }
| BOOL { Bool($1) }
| FLOAT { Float($1) }

exps:
| exp { $1 }
| exp exps {
  match $1 with
  | Let(a,b,Unit) -> Let(a,b, $2)
  | LetRec(a,Unit) -> LetRec(a, $2)
  | LetTuple(a, b, Unit) -> LetTuple(a, b, $2)
  | Unit -> $2
  | a -> Let(addtyp(Utils.genid("..")), a, $2)
}

elems:
| exp { [$1] }
| exp COMMA elems { $1 :: $3 }

exp_unary:
| exp_prim { $1 }
| NOT exp_unary { Not($2) }
| SUB exp {
  match $2 with
  | Float(f) -> Float(-.f) (* -1.23などは型エラーではないので別扱い *)
  | e -> Neg(e)
}
| SUB_DOT exp { FNeg($2) }

exp_post:
| exp_unary { $1 }
| IF LPAREN exp RPAREN exp ELSE exp { If($3, $5, $7) }
| ARRAY_CREATE LPAREN exp COMMA exp RPAREN { Array($3, $5) }
| exp_post LBRACKET exp RBRACKET { Get($1, $3) }
| exp_post LPAREN RPAREN { App($1, []) }
| exp_post LPAREN params RPAREN { App($1, $3) }
| exp_post LPAREN RPAREN VALEQ exp {
  match $1 with
  | Var(a) ->
    LetRec({name=addtyp a;args=[];body=$5}, Unit)
  | _ -> assert false
}
| exp_post LPAREN params RPAREN VALEQ exp {
  match $1 with
  | Var(a) ->
    let args = (List.map (function
      | Var(a) -> addtyp a
      | _ -> assert false
    ) $3) in
    LetRec({name=addtyp a;args=args;body=$6}, Unit)
  | _ -> assert false
}
params:
| exp { [$1] }
| exp COMMA params { $1::$3 }

exp_mul:
| exp_post { $1 }
| exp_mul MUL_DOT exp_post { FMul($1, $3) }
| exp_mul DIV_DOT exp_post { FDiv($1, $3) }

exp_add:
| exp_mul { $1 }
| exp_add ADD exp_mul { Add($1, $3) }
| exp_add SUB exp_mul { Sub($1, $3) }
| exp_add ADD_DOT exp_mul { FAdd($1, $3) }
| exp_add SUB_DOT exp_mul { FSub($1, $3) }

exp_lt:
| exp_add { $1 }
| exp_lt LT exp_add { Not(LE($3, $1)) }
| exp_lt GT exp_add { Not(LE($1, $3)) }
| exp_lt LE exp_add { LE($1, $3) }
| exp_lt GE exp_add { LE($3, $1) }

exp_eq:
| exp_lt { $1 }
| exp_eq EQ exp_lt { Eq($1, $3) }
| exp_eq NE exp_lt { Not(Eq($1, $3)) }

exp_val:
| exp_eq { $1 }
| exp_post VALEQ exp_eq {
  match $1 with
  | Var(a) ->
    Let(addtyp a, $3, Unit)
  | Tuple(ls) ->
    let ls = List.fold_right
      (fun t ls ->
        match t with
        | Var(a) -> (addtyp a) :: ls
        | Unit -> ls
        | _ -> 
          Format.fprintf Format.std_formatter "t=%a@." Syntax.print_t t;
          assert false
      )
      ls
      []
    in
    LetTuple(ls, $3, Unit)
  | t -> 
    Format.fprintf Format.std_formatter "t=%a@." Syntax.print_t t;
    assert false
}
| exp_post LBRACKET exp RBRACKET ASSIGN exp_eq { Put($1, $3, $6) }

exp:
| exp SEMICORON { $1 }
| exp_val { $1 }
