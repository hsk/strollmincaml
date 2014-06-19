# Float

## test.ml

テストを追加します:

```
  test("let a = (1.1,2) in let (b,c) = a in let d = b+.2.1-.0.1*.2.0/.1.2 in print(c)","(2\n,,0)");
  test("let a = (-. 1.1,2) in let (b,c) = a in let d = b+.2.1-.0.1*.2.0/.1.2 in print(c)","(2\n,,0)");
```

Floatは +. -. *. /. で四則演算し、マイナスを取るには -.を使います。

## syntax.ml

type tに以下を追加します:

```
  | Neg of t
  | Float of float
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | FNeg of t
```

print_tに以下を追加します:

```
  | Neg(a) -> fprintf ppf "Neg(%a)@?" print_t a
  | Float(f) -> fprintf ppf "Int(%f)@?" f
  | FAdd(a,b) -> fprintf ppf "FAdd(%a,%a)@?" print_t a print_t b
  | FSub(a,b) -> fprintf ppf "FSub(%a,%a)@?" print_t a print_t b
  | FMul(a,b) -> fprintf ppf "FMul(%a,%a)@?" print_t a print_t b
  | FDiv(a,b) -> fprintf ppf "FDiv(%a,%a)@?" print_t a print_t b
  | FNeg(a) -> fprintf ppf "FNeg(%a)@?" print_t a
```

## type.ml

type tに以下を追加します:

```
  | Float
```

print_tに以下を追加します:

```
  | Float -> fprintf ppf "Float@?"
```

## parser.mly

トークンを追加します:

```
%token <float> FLOAT
%token PLUS_DOT MINUS_DOT AST_DOT SLASH_DOT
```

優先順位を PLUS MINUSの所に追加します:

```
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST_DOT SLASH_DOT
%right prec_unary_minus
```

simple_expに以下を追加します:

```
| FLOAT
    { Float($1) }
```

expに以下を追加します:

```
| MINUS exp
    %prec prec_unary_minus
    { match $2 with
    | Float(f) -> Float(-.f) (* -1.23などは型エラーではないので別扱い *)
    | e -> Neg(e) }
| MINUS_DOT exp
    %prec prec_unary_minus
    { FNeg($2) }
| exp PLUS_DOT exp
    { FAdd($1, $3) }
| exp MINUS_DOT exp
    { FSub($1, $3) }
| exp AST_DOT exp
    { FMul($1, $3) }
| exp SLASH_DOT exp
    { FDiv($1, $3) }
```

## lexer.mll

以下のルールを追加します:

```
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| "-."
    { MINUS_DOT }
| "+."
    { PLUS_DOT }
| "*."
    { AST_DOT }
| "/."
    { SLASH_DOT }
```

## typing.ml

open Utilsの下に以下を追加します:

```
open Format
```

deref_termに以下を追加します:

```
  | Neg(e) -> Neg(deref_term e)
  | Float _ -> e
  | FAdd(e1, e2) -> FAdd(deref_term e1, deref_term e2)
  | FSub(e1, e2) -> FSub(deref_term e1, deref_term e2)
  | FMul(e1, e2) -> FMul(deref_term e1, deref_term e2)
  | FDiv(e1, e2) -> FDiv(deref_term e1, deref_term e2)
  | FNeg(e) -> FNeg(deref_term e)
```

unifyに以下を追加します:

```
  | Type.Float, Type.Float -> ()
```

inferに以下を追加します:

```
      | Neg(e1) ->
        unify Type.Int (infer env e1);
        Type.Int
      | Float(_) -> Type.Float
      | FAdd(e1, e2) | FSub(e1, e2) | FMul(e1, e2) | FDiv(e1, e2) ->
        unify Type.Float (infer env e1);
        unify Type.Float (infer env e2);
        Type.Float
      | FNeg(e1) ->
        unify Type.Float (infer env e1);
        Type.Float
```

エラー表示を分かりやすくする為に、applyに以下を追加します:

```
    | Error(a, t1, t2) ->
      fprintf str_formatter "error %a %a in %a"
        Type.print_t t1 
        Type.print_t t2
        Syntax.print_t a;
      failwith(flush_str_formatter())
```

## kNormal.ml

type tに以下を追加します:

```
  | Float of float
  | Neg of string
  | FAdd of string * string
  | FSub of string * string
  | FMul of string * string
  | FDiv of string * string
  | FNeg of string
```

print_tに以下を追加します:

```
  | Neg(a) -> fprintf ppf "Neg(\"%s\")" a
  | Float(f) -> fprintf ppf "Float(%f)@?" f
  | FAdd(a,b) -> fprintf ppf "FAdd(\"%s\",\"%s\")@?" a b
  | FSub(a,b) -> fprintf ppf "FSub(\"%s\",\"%s\")@?" a b
  | FMul(a,b) -> fprintf ppf "FMul(\"%s\",\"%s\")@?" a b
  | FDiv(a,b) -> fprintf ppf "FDiv(\"%s\",\"%s\")@?" a b
  | FNeg(a) -> fprintf ppf "FNeg(\"%s\")@?" a
```

visitに以下を追加します:

```
    | Syntax.Neg(e1) ->
      insert_let (visit env e1) (fun x ->
        (Neg(x), Type.Int)
      )
    | Syntax.Float(f) -> (Float(f), Type.Float)
    | Syntax.FAdd(e1, e2) ->
      insert_let (visit env e1) (fun x ->
        insert_let (visit env e2) (fun y ->
          (FAdd(x, y), Type.Float)
        )
      )
    | Syntax.FSub(e1, e2) ->
      insert_let (visit env e1) (fun x ->
        insert_let (visit env e2) (fun y ->
          (FSub(x, y), Type.Float)
        )
      )
    | Syntax.FMul(e1, e2) ->
      insert_let (visit env e1) (fun x ->
        insert_let (visit env e2) (fun y ->
          (FMul(x, y), Type.Float)
        )
      )
    | Syntax.FDiv(e1, e2) ->
      insert_let (visit env e1) (fun x ->
        insert_let (visit env e2) (fun y ->
          (FDiv(x, y), Type.Float)
        )
      )
    | Syntax.FNeg(e1) ->
      insert_let (visit env e1) (fun x ->
          (FNeg(x), Type.Float)
      )
```

## closure.ml

type tに以下を追加します:

```
  | Float of float
  | Neg of string
  | FAdd of string * string
  | FSub of string * string
  | FMul of string * string
  | FDiv of string * string
  | FNeg of string
```

print_tに以下を追加します:

```
  | Neg(a) -> fprintf ppf "Neg(\"%s\")" a
  | Float(f) -> fprintf ppf "Float(%f)@?" f
  | FAdd(a,b) -> fprintf ppf "FAdd(\"%s\",\"%s\")@?" a b
  | FSub(a,b) -> fprintf ppf "FSub(\"%s\",\"%s\")@?" a b
  | FMul(a,b) -> fprintf ppf "FMul(\"%s\",\"%s\")@?" a b
  | FDiv(a,b) -> fprintf ppf "FDiv(\"%s\",\"%s\")@?" a b
  | FNeg(a) -> fprintf ppf "FNeg(\"%s\")" a
```

freeVarに以下を追加します:

```
    | Neg(x) -> S.of_list [x]
    | Float(_) -> S.empty
    | FAdd(x, y)
    | FSub(x, y)
    | FMul(x, y)
    | FDiv(x, y) -> S.of_list [x; y]
    | FNeg(x) -> S.of_list [x]
```

visitに以下を追加します:

```
  | KNormal.Neg(x) -> Neg(x)
  | KNormal.Float(d) -> Float(d)
  | KNormal.FAdd(x, y) -> FAdd(x, y)
  | KNormal.FSub(x, y) -> FSub(x, y)
  | KNormal.FMul(x, y) -> FMul(x, y)
  | KNormal.FDiv(x, y) -> FDiv(x, y)
  | KNormal.FNeg(x) -> FNeg(x)
```

## virtual.ml

visitに以下を追加します:

```
    | Closure.Neg(aId) ->
      let aR = M.find aId env in
      let bR = RN(Type.Int, "0") in
      let retR = RL(regt aR,genid("..")) in
      add(Bin(retR, "sub", aR, bR));
      retR
    | Closure.Float(f) ->
      RN(Type.Float, string_of_float f)
    | Closure.FAdd(x, y) -> bin env "fadd" x y 
    | Closure.FSub(x, y) -> bin env "fsub" x y
    | Closure.FMul(x, y) -> bin env "fmul" x y
    | Closure.FDiv(x, y) -> bin env "fdiv" x y
    | Closure.FNeg(aId) ->
      let aR = M.find aId env in
      let bR = RN(Type.Float, "0.0") in
      let retR = RL(regt aR,genid("..")) in
      add(Bin(retR, "fsub", aR, bR));
      retR
```

## emit.ml

ptにFloat型を追加します:

```
  | Type.Float -> "double"
```
