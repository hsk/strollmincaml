# 配列

MinCamlの配列は配列のポインタを持って回る作りです。
Array.createで長さと初期値を指定して配列を作成し、
初期値が必ず入っている配列を準備して使います。

## test.ml

テストを追加します:

```
  test("let a = Array.create 2 112 in print(a.(1));a.(0)<-2;print(a.(0))","(112\n2\n,,0)");
```

Array.createで作成し、a.(1)等でアクセスします。

## main.ml

ソースを書き換えます。

```
  let src = "let a = Array.create 2 112 in print(a.(1));a.(0)<-2;print(a.(0))" in
```

## type.ml

type t のFunの下に以下を追加します。

```
  | Array of t
```

print_tに以下を追加します。

```
  | Array(t) -> fprintf ppf "Array(%a)@?" print_t t
```

## syntax.ml

type tに以下を追加します。

```
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
```

print_tに以下を追加します。

```
  | Array(a,b) -> fprintf ppf "Array(%a,%a)@?" print_t a print_t b
  | Get(a,b) -> fprintf ppf "Get(%a,%a)@?" print_t a print_t b
  | Put(a,b,c) -> fprintf ppf "Put(%a,%a,%a)@?" print_t a print_t b  print_t c
```

## parser.mly

トークンを追加します。

```
%token DOT LESS_MINUS ARRAY_CREATE
```

優先順位

```
%right LESS_MINUS
```

をprec_ifの後ろにを追加します。

simple_expに以下を追加します。

```
| simple_exp DOT LPAREN exp RPAREN
    { Get($1, $4) }
```

expに以下を追加します。

```
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp
    { Put($1, $4, $7) }
| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app
    { Array($2, $3) }
```

## lexer.mll

字句解析のルールに以下を追加します:

```
| "Array.create" (* [XX] ad hoc *)
    { ARRAY_CREATE }
| '.'
    { DOT }
| "<-"
    { LESS_MINUS }
```

## typing.ml

deref_typeに以下を追加します。

```
    | Type.Array(t) -> Type.Array(deref_type t)
```

deref_termに以下を追加します。

```
  | Array(e1, e2) -> Array(deref_term e1, deref_term e2)
  | Get(e1, e2) -> Get(deref_term e1, deref_term e2)
  | Put(e1, e2, e3) -> Put(deref_term e1, deref_term e2, deref_term e3)
```

occurに以下を追加します。

```
    | Type.Array(t2) -> occur r1 t2
```

unifyに以下を追加します。

```
  | Type.Array(t1), Type.Array(t2) -> unify t1 t2
```

inferに以下を追加します。

```
      | Array(e1, e2) ->
        unify (infer env e1) Type.Int;
        Type.Array(infer env e2)
      | Get(e1, e2) ->
        let t = Type.gentyp () in
        unify (Type.Array(t)) (infer env e1);
        unify Type.Int (infer env e2);
        t
      | Put(e1, e2, e3) ->
        let t = infer env e3 in
        unify (Type.Array(t)) (infer env e1);
        unify Type.Int (infer env e2);
        Type.Unit
```

## kNormal.ml

type tに以下を追加します:

```
  | Get of string * string
  | Put of string * string * string
  | ExtArray of string * Type.t
```

print_tに以下を追加します:

```
  | Get(a,b) -> fprintf ppf "Get(\"%s\",\"%s\")@?" a b
  | Put(a,b,c) -> fprintf ppf "Put(\"%s\",\"%s\",\"%s\")@?" a b c
  | ExtArray(s,t) -> fprintf ppf "ExtArray(\"%s\",%a)@?" s Type.print_t t
```

visitのSyntax.Varの箇所を外部配列の参照が出来るように修正します:

TODO:テスト

```
    | Syntax.Var(x) -> (* 外部配列の参照 (caml2html: knormal_extarray) *)
      (match M.find x !Typing.extenv with
      | Type.Array(_) as t -> ExtArray(x, t), t
      | _ -> failwith (Printf.sprintf "external variable %s does not have an array type" x))
```

visitに以下を追加します:

```
    | Syntax.Array(e1, e2) ->
      insert_let (visit env e1) (fun x ->
        let _, t2 as g_e2 = visit env e2 in
        insert_let g_e2 (fun y ->
          let l =
            match t2 with
            (*| Type.Float -> "create_float_array" *)
            | _ -> "create_array" in
          ExtFunApp(l, [x; y], Type.Array(t2)), Type.Array(t2)
        )
      )
    | Syntax.Get(e1, e2) ->
      (match visit env e1 with
      | _, Type.Array(t) as g_e1 ->
        insert_let g_e1 (fun x ->
          insert_let (visit env e2) (fun y ->
            Get(x, y), t
          )
        )
      | _ -> assert false
      )
    | Syntax.Put(e1, e2, e3) ->
      insert_let (visit env e1) (fun x ->
        insert_let (visit env e2) (fun y ->
          insert_let (visit env e3) (fun z ->
            Put(x, y, z), Type.Unit)
        )
      )
```

## closure.ml

type tに以下を追加します:

```
  | Get of string * string
  | Put of string * string * string
  | ExtArray of string * Type.t
```

print_tに以下を追加します:

```
  | Get(a,b) -> fprintf ppf "Get(\"%s\",\"%s\")@?" a b
  | Put(a,b,c) -> fprintf ppf "Put(\"%s\",\"%s\",\"%s\")@?" a b c
  | ExtArray(s,t) -> fprintf ppf "ExtArray(\"%s\",%a)@?" s Type.print_t t
```

freeVarsに以下を追加します:

```
    | Get(x, y) -> S.of_list [x; y]
    | Put(x, y, z) -> S.of_list [x; y; z]
    | ExtArray(_,_) -> S.empty
```

visitに以下を追加します:

```
  | KNormal.Get(x, y) -> Get(x, y)
  | KNormal.Put(x, y, z) -> Put(x, y, z)
  | KNormal.ExtArray(x, t) -> ExtArray(x, t)
```

## virtual.ml

type tに以下を追加します:

```
  | Field of r * r * r
  | Load of r * r
  | Store of r * r
```

visitに以下を追加します:

```
    | Closure.Get(x, y) ->
      let xr = M.find x env in
      let xt = regt xr in
      (match xt with
        | Type.Array(Type.Unit) -> RN(Type.Unit, "0")
        | Type.Array(t) ->
          let reg5 = RL(t, genid("..")) in
          let reg4 = RL(Type.Array(t), genid("..")) in
          add(Field(reg4, xr, M.find y env));
          add(Load(reg5, reg4));
          reg5
        | t ->
          fprintf str_formatter "x=%s t=%a" x Type.print_t t;
          failwith (flush_str_formatter())
      )
    | Closure.Put(x, y, z) ->
      let xr = M.find x env in
      let xt = regt xr in
      (match xt with
        | Type.Array(Type.Unit) -> RN(Type.Unit, "0")
        | Type.Array(t)  ->

          let reg4 = RL(Type.Array(t), genid("..")) in
          add(Field(reg4, xr, M.find y env));
          let reg5 = M.find z env in
          add(Store(reg5, reg4));
          reg5
        | t ->
          fprintf str_formatter "x=%s t=%a" x Type.print_t t;
          failwith (flush_str_formatter())
      )
    | Closure.ExtArray(x, t) -> RG(t, "min_caml_" ^ x)
```

## emit.ml

ptにのFunの下にArray型を追加します:

```
  | Type.Array(t) -> pt t ^ "*"
```

emitに Load,Store,Fieldを追加します:

```
    | Load(reg1, reg2) ->
      asm_p(p(reg1) ^ " = load " ^ pr(reg2))
    | Store(reg1, reg2) ->
      asm_p("store " ^ pr(reg1) ^ ", " ^ pr(reg2))
    | Field(a, b, c) ->
      asm_p(p a ^ " = getelementptr inbounds " ^ pr b ^ ", " ^ pr c)
```

applyにcreate_arrayの実装を追加し、mallocの定義も追加します:

```
  asm("define i64* @create_array(i64 %size, i64 %init) {");
  asm("entry:");
  asm_p("%size1 = mul nsw i64 %size, 8");
  asm_p("%mem1 = call i8* @malloc(i64 %size1)");
  asm_p("%mem = bitcast i8* %mem1 to i64*");
  asm_p("br label %loop");
  asm("");
  asm("loop:");
  asm_p("%i.0 = phi i64 [ %size, %entry ], [ %i.1, %body ]");
  asm_p("%l5 = icmp sgt i64 %i.0, 0");
  asm_p("br i1 %l5, label %body, label %end");
  asm("");
  asm("body:");
  asm_p("%i.1 = sub nsw i64 %i.0, 1");
  asm_p("%addr = getelementptr inbounds i64* %mem, i64 %i.1");
  asm_p("store i64 %init, i64* %addr, align 8");
  asm_p("br label %loop");
  asm("end:");
  asm_p("ret i64* %mem");
  asm("}");

  asm("declare i8* @malloc(i64)");
```

omake omake testが通れば完了です。
