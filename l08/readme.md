# BoolとIf

## test.ml

テストを追加します。

```
  test("let a = 1 = 1 in if a then print 1 else print 2;if 1=0 then print 1 else print 2","(1\n2\n,,0)");
  test("
    let rec f a n =
      if n = 0 then a
      else f (a+n) (n-1)
    in print(f 0 10)
  ", "(55\n,,0)");
  test("
    let rec fib n =
      if n < 1 then 0
      else if n = 1 then 1
      else fib (n - 1) + fib (n - 2)
    in print(fib 30)
  ", "(832040\n,,0)");
```

if文が使えるようにする訳ですね。

## type.ml

type tのIntの下にBoolを追加します:
```
  | Bool
```

print_tにもIntの下にBoolを追加します:

```
  | Bool -> fprintf ppf "Bool@?"
```

## syntax.ml

type tのSubの下に Bool等を追加します:

```
  | Bool of bool
  | Not of t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
```


print_tのSubの下に以下を追加します:

```
  | Bool(b) -> fprintf ppf "Bool(%b)@?" b
  | Not(t) -> fprintf ppf "Not(%a)" print_t t
  | Eq(a,b) -> fprintf ppf "Eq(%a,%a)@?" print_t a print_t b
  | LE(a,b) -> fprintf ppf "LE(%a,%a)@?" print_t a print_t b
  | If(a,b,c) -> fprintf ppf "If(%a,%a,%a)@?" print_t a print_t b print_t c
```

## parser.mly

<string>の下にトークンを追加します。

```
%token <bool> BOOL
%token NOT LESS_GREATER LESS GREATER GREATER_EQUAL LESS_EQUAL
%token IF THEN ELSE
```

%left EQUALの箇所を以下に書き換えます。

```
%right prec_if
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
```

simple_expのINTの下にBOOLを追加します。

```
| BOOL
    { Bool($1) }
```

expのexp MINUS expの下に以下を追加します:
```
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
```


## lexer.mll

以下のルールを追加します:

```
| "true"
    { BOOL(true) }
| "false"
    { BOOL(false) }
| "not"
    { NOT }
| "<>"
    { LESS_GREATER }
| "<="
    { LESS_EQUAL }
| ">="
    { GREATER_EQUAL }
| '<'
    { LESS }
| '>'
    { GREATER }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
```

## typing.ml

deref_termの以下の箇所を


```
  | Unit | Var _ | Int _ -> e
```

以下に変更、追加します:

```
  | Unit | Var _ | Int _ | Bool _ -> e
  | Not(e) -> Not(deref_term e)
  | Eq(e1, e2) -> Eq(deref_term e1, deref_term e2)
  | LE(e1, e2) -> LE(deref_term e1, deref_term e2)
  | If(e1, e2, e3) -> If(deref_term e1, deref_term e2, deref_term e3)
```


unifyの以下の箇所に

```
  | (Type.Unit, Type.Unit) | (Type.Int, Type.Int) -> ()
```

Boolを加えて以下のようにします:

```
  | Type.Unit, Type.Unit | Type.Bool, Type.Bool | Type.Int, Type.Int -> ()
```

inferに以下を追加します:

```
      | Bool(_) -> Type.Bool
      | Not(e) ->
        unify Type.Bool (infer env e);
        Type.Bool
      | Eq(e1, e2) ->
        unify (infer env e1) (infer env e2);
        Type.Bool
      | LE (e1, e2) ->
        unify (infer env e1) (infer env e2);
        Type.Bool
      | If(e1, e2, e3) ->
        unify (infer env e1) Type.Bool;
        let t2 = infer env e2 in
        let t3 = infer env e3 in
        unify t2 t3;
        t2
```

## kNormal.ml

type tに以下を追加します:

```
  | Bool of bool
  | If of string * t * t
  | LE of string * string
  | Eq of string * string
```

print_tに以下を追加します:

```
  | Bool(b) ->  fprintf ppf "Bool(%b)@?" b
  | If(s,a,b) -> fprintf ppf "If(\"%s\",%a,%a)" s print_t a print_t b
  | Eq(a,b) -> fprintf ppf "Eq(\"%s\",\"%s\")@?" a b
  | LE(a,b) -> fprintf ppf "LE(\"%s\",\"%s\")@?" a b
```

visitに以下を追加します:

```
    | Syntax.Bool(b) -> Bool b, Type.Bool
    | Syntax.Not(e) ->
      visit env (Syntax.If(e, Syntax.Bool(false), Syntax.Bool(true)))
    | Syntax.Eq (e1, e2) ->
      insert_let (visit env e1) (fun x ->
        insert_let (visit env e2) (fun y ->
          (Eq(x,y), Type.Bool)
        )
      )
    | Syntax.LE (e1, e2) ->
      insert_let (visit env e1) (fun x ->
        insert_let (visit env e2) (fun y ->
          (LE(x,y), Type.Bool)
        )
      )
    | Syntax.If(e1, e2, e3) ->
      insert_let (visit env e1) (fun x ->
        let e2', t2 = visit env e2 in
        let e3', t3 = visit env e3 in
        If (x, e2', e3'), t2
      )
```

## closure.ml

type tに以下を追加します:

```
  | Bool of bool
  | If of string * t * t
  | LE of string * string
  | Eq of string * string
```

print_tに以下を追加します:

```
  | Bool(b) -> fprintf ppf "Bool(%b)@?" b
  | If(s,a,b) -> fprintf ppf "If(\"%s\",%a,%a)" s print_t a print_t b
  | Eq(a,b) -> fprintf ppf "Eq(\"%s\",\"%s\")@?" a b
  | LE(a,b) -> fprintf ppf "LE(\"%s\",\"%s\")@?" a b
```

freeVarに以下を追加します:

```
    | Bool(_) -> S.empty
    | Eq(x, y) | LE(x, y) -> S.of_list [x; y]
    | If(x, a, b) -> S.add x (S.union (freeVar a) (freeVar b))
```

visitに以下を追加します:

```
  | KNormal.Bool(b) -> Bool(b)
  | KNormal.If(x, e1, e2) -> If(x, visit env known e1, visit env known e2)
  | KNormal.Eq(e1, e2) -> Eq(e1, e2)
  | KNormal.LE(e1, e2) -> LE(e1, e2)
```

## virtual.ml

type tに以下を追加します:

```
  (* 条件分岐 *)
  | Jne of r * string * string * string
  (* ジャンプ命令 *)
  | Goto of string * string
  (* ラベル命令 *)
  | Label of string * string
  (* SSA最適化のφ of ファイ 複数基本ブロックの変数を１つに合流させる *)
  | Phi of r * string * string * Type.t * r * r
```

visitに以下を追加します:

```
    | Closure.Bool(b) ->
      RN(Type.Bool, if b then "-1" else "0")
    | Closure.Eq(x, y) -> bin env "eq" x y
    | Closure.LE(x, y) -> bin env "le" x y
    | Closure.If(x, e1, e2) ->
      let id1 = genid("ok") in
      let (id2, l1) = (genid("else"), genid("else")) in
      let (id3, l2) = (genid("endif"), genid("endif")) in
      let rx = M.find x env in (* cond *)
      add(Jne(rx, id1, id1, id2));
      let r1 = visit env e1 in
      add(Label(l1, l1));
      add(Goto(id2, id3));
      let r2 = visit env e2 in
      add(Label(l2, l2));
      add(Goto(id3, id3));
      if ((regt r1) <> Type.Unit && (regt r1) = (regt r2))
      then (
        let r3 = RL(regt r1, genid("..")) in
        add(Phi(r3, l1, l2, regt r1, r1, r2));
        r3
      ) else
        RN(Type.Unit, "0")
```

## emit.ml

ptにboolを追加します:

```
  | Type.Bool -> "i1"
```

emitのBinの箇所をeqとleに対応します:

```
    | Bin(id, op, a, b) ->
      (match op with
      | "eq" | "ne" ->
          let reg1 = RL(Type.Bool,genid("..")) in
          asm_p(p(reg1) ^ " = icmp "^ op ^ " " ^ pr(a) ^ ", " ^ p(b));
          asm_p(p(id) ^ " = sext " ^ pr(reg1) ^ " to "^ptr(id));
      | "lt"|"le"|"gt"|"ge"->
          let reg1 = RL(Type.Bool,genid("..")) in
          asm_p(p(reg1) ^ " = icmp s"^ op ^ " " ^ pr(a) ^ ", " ^ p(b));
          asm_p(p(id) ^ " = sext i1 " ^ p(reg1) ^ " to "^ptr(id))
      | _ ->
        asm_p(p(id) ^ " = " ^ op ^ " " ^ pr a ^ ", " ^ p(b))
      )
```

emit に Jne, Goto, Label, Phiを追加します:

```
    | Jne(r, label, jmp1, jmp2) ->
      let reg = genid("%reg_") in
      asm_p(reg ^ " = icmp ne " ^ ptr(r) ^ " " ^ p(r) ^ ", 0");
      asm_p("br i1 " ^ reg ^ ", label %" ^ jmp1 ^ ", label %" ^ jmp2);
      asm(label ^ ":")
    | Goto(label, jmp) ->
      asm_p("br label %" ^ jmp);
      if (label <> "") then asm(label ^ ":") else ()
    | Label(jmp, label) ->
      if (jmp <> "") then asm_p("br label %" ^ jmp) else ();
      asm(label ^ ":")
    | Phi(r, l1, l2, t, r1, r2) ->
      asm_p(p(r) ^ " = phi " ^ pt(t) ^ " [" ^ p(r1) ^ ", %" ^ l1 ^ "], [" ^ p(r2) ^ ", %" ^ l2 ^ "]")
```

分量が多かったような気もしますが、
omake omake testで問題なければ完了です。

