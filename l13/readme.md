# C言語風の構文

この章では、実験的にパーサをC言語風に書き換えます。

## test.ml

testを新しい文法で書き換えます:

```
  test("{print(1)print(2 + 3)print((2+3)-2)}","(1\n5\n3\n,,0)");
  test("{ a := 1+2; print(a)}","(3\n,,0)");
  test("{a := {b := 1+2; b} print(a)}","(3\n,,0)");
  test("{a := {b := 1+2; print(b); b } print(a)}","(3\n3\n,,0)");
  test("
  {
    f(x) := x + 1
    print(f(1))
    print(2 + 3)
    print(2 + 3 - 2)
    a := 1+2
    print(a)
  }
  ", "(2\n5\n3\n3\n,,0)");
  test("{f(x):={e(y):=x+y print(1)e}print(f(2)(1))}","(1\n3\n,,0)");
  test("{f(x):={e(y):=x+y print(e(1))}f(2)}","(3\n,,0)");
  test("{a := 1 == 1 if(a)print(1)else print(2) if (1==0) print(1) else print(2)}","(1\n2\n,,0)");
  test("{
    f(a, n) :=
      if (n == 0) a
      else f(a+n, n-1)
    print(f(0, 10))}
  ", "(55\n,,0)");
  test("{
    fib(n):=
      if (n < 1) 0
      else if (n == 1) 1
      else fib (n - 1) + fib (n - 2)
    print(fib(30))
  }", "(832040\n,,0)");
  test("{a := Array.create(2, 112) print(a[1])a[0]=2 print(a[0])}","(112\n2\n,,0)");
  test("{a := (1,2); (b,c) := a print(b) print(c)}", "(1\n2\n,,0)");
  test("{a := (1.1,2); (b,c) := a d := b+.2.1-.0.1*.2.0/.1.2 print(c)}","(2\n,,0)");
  test("{a := (-. 1.1,2); (b,c) := a d := b+.2.1-.0.1*.2.0/.1.2 print(c)}","(2\n,,0)");
```

:=をなぜ、=としなかったかというのがおそらく気になる所だと思います。
その質問の答えは

:と=の間に型を書くと

```
a:int=1
```

と明示的に型を書く拡張を考えているからです。
さらに、:を#に書き換えるとmutableにする事も考えています:

```
a#int=1
```

mutableの際の型を省略した場合は以下のように書きます。

```
a#=1
```

このような理由で:=で値の束縛を行うようにしました。

## parser.mly

パーサの実装は以下のように書き換えます:

```
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
```

最も重要なポイントは、expsの文法です。Let(a,b,c)のcが空ならそこに入れるという操作をする事でブロックの構文から、let in形式に書き換えています。


## lexer.mll

字句解析は以下のように書きかえます。

```
{
open Parser
}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let digit = ['0'-'9']

rule token = parse
| [' ' '\n' '\r' '\t'] { token lexbuf }
| digit+
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| "-."           { SUB_DOT }
| "+."           { ADD_DOT }
| "*."           { MUL_DOT }
| "/."           { DIV_DOT }
| '+'            { ADD }
| '-'            { SUB }
| '('            { LPAREN }
| ')'            { RPAREN }
| '{'            { LBRACE }
| '}'            { RBRACE }
| ';'            { SEMICORON }
| ":="           { VALEQ }
| '_'            { IDENT(Utils.genid("..")) }
| ','            { COMMA }
| "true" { BOOL(true) }
| "false" { BOOL(false) }
| '!' { NOT }
| "!=" { NE }
| "==" { EQ }
| "<=" { LE }
| ">=" { GE }
| '<'  { LT }
| '>'  { GT }
| "if" { IF }
| "else" { ELSE }
| "Array.create" { ARRAY_CREATE }
| '[' { LBRACKET }
| ']' { RBRACKET }
| '=' { ASSIGN }
| lower (digit|lower|upper|'_')*  { IDENT(Lexing.lexeme lexbuf) }
| eof            { EOF }
```
