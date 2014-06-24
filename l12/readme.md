# 末尾再帰最適化

LLVMだとcallをtail callと変えるだけであとは、末尾再帰にしてくれます。
なので、末尾再帰最適化はtail callを末尾だったらtailと付ければ大体はうまく行く訳です。

変更するのは、Virtualです。

今までのvisitをvisit_tailとvisitに分けて関数本体からはvisit_tailを呼び、
末尾を検索します。末尾再帰最適化出来ないか、必要ない式はvisitを呼び出すようにします。

## main.ml

仕様自体は変わらないのですが、

```
  let src = "
    let rec f a n =
      if n = 0 then a
      else f (a+n) (n-1)
    in print(f 0 10)" in
```

こんなプログラムをコンパイルしたときに、a.llにtail callが追加され、
a.sではジャンプ命令に変換されている事を確認するとよいので、書き換えて実行してみましょう。

通常のビルドだと、a.llやa.sは直ぐ消してしまうので、

```
$ omake
$ ./lllc
```

と直接./lllcを実行してa.s a.llを残して見てみましょう。

変更前のa.llの@f内の@fの呼び出しは只のcallです。

```
  %..17 = call i64 @f(i64 %..15, i64 %..16) nounwind ssp
```

a.s内の_f:以下の再帰呼び出しの箇所は例えば以下のようにcallqです。
```
  callq _f
```

末尾再帰最適化の処理を加えると、a.llの@fは以下のようにtail callにtailが付き、

```
  %..14 = tail call i64 @f(i64 %..12, i64 %..13) nounwind ssp
```

a.sは例えば以下のようになジャンプ命令に変換されます。

```
  jmp _f                      ## TAILCALL
```

このようなプログラムにしていきましょう。

## virtual.ml

type tのCallにboolを付け加えます。このboolはtail callかどうかを表します。

```
  | Call of bool * r * r * r list
```

visitのExtFunAppの関数呼び出しにtail call のフラグを追加します。

```
          add(Call(false, retR, nameR, prmRs));
```

AppDir、AppCls,LetTupleの実装を２カ所で共有したいので実装を関数呼び出しにします。
元の実装は関数の下にカット＆ペーストで移動します。

AppDirの実装

```
    | Closure.AppDir(nameId, prmIds) ->
      app_dir env false nameId prmIds
```

AppClsの実装

```
    | Closure.AppCls(nameId, prmIds) ->
      app_cls env false nameId prmIds
```

LetTupleの実装

```
    | Closure.LetTuple (atl, a, e) ->
      let env = let_tuple env false atl a in
      visit env e
```

外に出した関数app_dir,app_cls,let_tupleをコピーして追加します。
Callにtailを付けて作成します。

```
and app_dir env tail nameId prmIds =
  try
    let prmRs = List.map (fun prmId -> M.find prmId env ) prmIds in
    let nameR = M.find nameId env in
    let retR = RL(regt nameR, genid("..")) in
    add(Call(tail, retR, nameR, prmRs));
    retR
  with
    Not_found ->
      failwith ("not found appdir "^ nameId)

and app_cls env tail closureId prmIds =
  let funPrmRs = List.map
    (fun prmId -> M.find prmId env )
    prmIds
  in
  let closureR = M.find closureId env in

  let (funR, closurePrmRs, retT) =
    match regt closureR with
    | Type.Tuple((Type.Fun(_, retT) as funT)::prmTs) -> 
      let funR = RL(funT, genid("..")) in
      add(ExtractValue(funR, closureR, 0));
      let (closurePrmRs,_) = List.fold_left
        ( fun (closurePrmRs, closureIndex) closurePrmT ->
          let closurePrmR = RL(closurePrmT, genid("..")) in
          add(ExtractValue(closurePrmR, closureR, closureIndex));
          (closurePrmR::closurePrmRs, closureIndex + 1)
        )
        ([],1)
        prmTs
      in
      (funR, List.rev closurePrmRs, retT)
    | _ ->
      fprintf str_formatter "error id=%s t=%a" closureId Type.print_t (regt closureR);
      failwith(flush_str_formatter())
  in

  let retR = RL(retT, genid("..")) in
  add(Call(tail, retR, funR, closurePrmRs @ funPrmRs));
  retR

and let_tuple env tail atl a =
  let ar = M.find a env in
  let (env,_ ) = List.fold_left
    (fun (env,i) (id1,t) ->
      let r = RL(t,id1) in
      add(ExtractValue(r, ar, i));
      (M.add id1 r env, i+1)
    )
    (env, 0)
    atl
  in env
```

visit_tailは関数内の末尾となりうる式のときだけ呼び出します。
If,Let,LetTupleではそれぞれさらに継続があるのでvisit_tailを呼び出します。
AppDir,AppClsは関数呼び出しで、tail_call出来るのでtail callにして、Retを付けます。
その他の式は終了する式なので、visitを呼び出して、やはりRetを付けます。

```
let rec visit_tail env e =
  match e with
  | Closure.If(x, e1, e2) ->
    let id1 = genid("ok") in
    let id2 = genid("else") in
    let rx = M.find x env in (* cond *)
    add(Jne(rx, id1, id1, id2));
    let r1 = visit_tail env e1 in
    add(Label(id2, id2));
    let r2 = visit_tail env e2 in
    if ((regt r1) <> Type.Unit && (regt r1) = (regt r2))
    then r1
    else RN(Type.Unit, "0")

  | Closure.Let ((id, _), e1, e2) ->
      let v = visit env e1 in
      visit_tail (M.add id v env) e2
  | Closure.AppDir(nameId, prmIds) ->
    let retR = app_dir env true nameId prmIds in
    add(Ret(retR));
    retR
  (* クロージャ実行 *)
  | Closure.AppCls(nameId, prmIds) ->
    let retR = app_cls env true nameId prmIds in
    add(Ret(retR));
    retR
  | Closure.LetTuple (atl, a, e) ->
    let env = let_tuple env true atl a in
    visit_tail env e
  | _ ->
    let v = visit env e in
    add(Ret(v));
    v
```

visitfunからRet(r)を(visit_tailでRetは付けているので)消します:

```
    add(Ret(r));
```

visitの呼び出しをvisit_tailに変えます。

```
    let r = visit_tail env' e in
```

## emit.ml

emitのCallにtailを追加し、tailの文字列を作ります。

```
    | Call(tail, id, r, prms) ->
      let tail = if tail then "tail " else "" in
```

後は、callの手前にtailを付け加えて完了です。

```
          asm_p(tail ^ "call " ^ pr(r) ^ "(" ^ ps ^ ") nounwind ssp")
```

=がある場合もtailを付け加えます。

```
          asm_p(p(id) ^ " = " ^ tail ^ "call " ^ pr(r) ^ "(" ^ ps ^ ") nounwind ssp")
```

## 確認

```
$ omake test
```

で、全体の動作がおかしくなっていないかを確認します。

```
$ omake
$ ./lllc
```

と直接./lllcを実行してa.s a.llを残して見てみましょう。

a.llの@fは以下のようにtail callにtailが付き、

```
  %..14 = tail call i64 @f(i64 %..12, i64 %..13) nounwind ssp
```

a.sは例えば以下のようになジャンプ命令に変換してあれば成功です。

```
  jmp _f                      ## TAILCALL
```

