# 多値

## main.ml

テストを変更します。

```
  let src = "let a = (1,2) in let (b,c) = a in print(b+c)" in
```

## test.ml

テストを追加します。

```
  test("let a = (1,2) in let (b,c) = a in print(b); print(c)", "(1\n2\n,,0)");
```

多値は(1,2)と言った形式で初期化して、let(b,c)=aのような形で取り出して使います。

## syntax.ml

type tに以下を追加します:

```
  | Tuple of t list
  | LetTuple of (string * Type.t) list * t * t
```

print_tに以下を追加します:

```
  | Tuple(ts) -> fprintf ppf "Tuple(%a)" print_ts ts
  | LetTuple(sts,e1,e2) ->
    fprintf ppf "LetTuple(%a,%a,%a)"
      print_sts sts print_t e1 print_t e2
```

## parser.mly

以下のトークンを追加します:

```
%token COMMA
```

%right LESS_MINUSの下に以下の優先順位を追加します:

```
%left COMMA
```

expに以下を追加します:

```
| elems
    { Tuple($1) }
| LET LPAREN pat RPAREN EQUAL exp IN exp
    { LetTuple($3, $6, $8) }
```

expの後ろに

```
elems:
| elems COMMA exp
    { $1 @ [$3] }
| exp COMMA exp
    { [$1; $3] }

pat:
| pat COMMA IDENT
    { $1 @ [addtyp $3] }
| IDENT COMMA IDENT
    { [addtyp $1; addtyp $3] }
```
の２つの構文要素を追加します。

## lexer.ml

```
| ','
    { COMMA }
```

を追加します。

## typing.ml

deref_typeに以下を追加します:

```
    | Type.Tuple(ts) -> Type.Tuple(List.map deref_type ts)
```

deref_termに以下を追加します:

```
  | Tuple(es) -> Tuple(List.map deref_term es)
  | LetTuple(xts, e1, e2) -> LetTuple(List.map deref_id_type xts, deref_term e1, deref_term e2)
```

occurに以下を追加します:

```
    | Type.Tuple(t2s) -> List.exists (occur r1) t2s
```

unifyに以下を追加します:

```
  | Type.Tuple(t1s), Type.Tuple(t2s) ->
    (try
      List.iter2 unify t1s t2s
    with
      | Invalid_argument("List.iter2") -> raise (Unify(t1, t2))
    )
```

inferに以下を追加します:

```
      | Tuple(es) -> Type.Tuple(List.map (infer env) es)
      | LetTuple(xts, e1, e2) ->
        unify (Type.Tuple(List.map snd xts)) (infer env e1);
        infer (M.add_list xts env) e2
```

## kNormal.ml
type tに以下を追加します:

```
  | LetTuple of (string * Type.t) list * string * t
  | Tuple of string list
```

print_tに以下を追加します:

```
  | LetTuple(sts,s,t) ->
    fprintf ppf "LetTuple(%a,\"%s\",%a)@?"
      Syntax.print_sts sts
      s print_t t
  | Tuple(ss) -> fprintf ppf "Tuple([%s])@?" (String.concat "; " ss)
```

visitに以下を追加します:

```
    | Syntax.Tuple(es) ->
      let rec bind (xs:string list) (ts:Type.t list) = function (* "xs" and "ts" are identifiers and types for the elements *)
        | [] -> Tuple(List.rev xs), Type.Tuple(List.rev ts)
        | e :: es ->
          let _, t as g_e = visit env e in
          insert_let g_e (fun x ->
            bind (x::xs) (t::ts) es
          )
      in
        bind [] [] es
    | Syntax.LetTuple(xts, e1, e2) ->
      insert_let (visit env e1) (fun y ->
        let e2', t2 = visit (M.add_list xts env) e2 in
        LetTuple(xts, y, e2'), t2
      )
```

## closure.ml

type tに以下を追加します:

```
  | LetTuple of (string * Type.t) list * string * t
  | Tuple of string list
```

print_tに以下を追加します:

```
  | LetTuple(sts,s,t) ->
    fprintf ppf "LetTuple(%a,\"%s\",%a)@?"
      Syntax.print_sts sts
      s print_t t
  | Tuple(ss) -> fprintf ppf "Tuple([%s])@?" (String.concat "; " ss)
```

freeVarに以下を追加します:

```
    | LetTuple(xts, y, e) -> S.add y (S.diff (freeVar e) (S.of_list (List.map fst xts)))
    | Tuple(ss) -> S.of_list ss
```

visitに以下を追加します:

```
  | KNormal.LetTuple(xts, y, e) ->
    LetTuple(xts, y, visit (M.add_list xts env) known e)
  | KNormal.Tuple(xs) -> Tuple(xs)
```

## virtual.ml

visitに以下を追加します:

```
    | Closure.LetTuple (atl, a, e) ->
        let ar = M.find a env in
        let (env,_ ) = List.fold_left
          (fun (env,i) (id1,t) ->
            let r = RL(t,id1) in
            add(ExtractValue(r, ar, i));
            (M.add id1 r env, i+1)
          )
          (env, 0)
          atl
        in
        visit env e
    | Closure.Tuple (xs:string list) ->
      let ts = List.map (fun x -> regt (M.find x env)) xs in
      let t = Type.Tuple(ts) in

      let (r,_) = List.fold_left
        (fun ((src:r),(n:int)) (x:string) ->
          (* 構造体 *)
          let dst = RL(t, genid("..")) in
          let r = M.find x env in
          add(InsertValue(dst, src, r, n));
          (dst,n+1)
        )
        (RN(t,"undef"), 0)
        xs
      in r
```

## emit.ml

変更無し
