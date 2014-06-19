# k正規化 KNormal

前の章では、２パスでコンパイルしていました。
この章ではvirtualをkNormalとvirtualの２つに分けて３パス構成にします。

kNormalはk正規化を行い、k正規形に変換する処理です。
k正規形とはネストした式を平たくした物です。
例えば、1+2+3をk正規形で表すと let a = 1 + 2 in a + 3と表せます。
k正規化すると式のネストが下がるため、以降のパスの再帰呼び出しのネストが減り末尾再帰最適化でスタックをあまり食わなく等のメリットがあります。

virtualの変更はSyntax.tをVirtual.tに変換 -> KNormal.tからVirtual.tに変換するようにします。

それでは作成していきましょう。

## main.ml

VirtualはKNormalとVritualに分けます。

## KNormalモジュール

KNormalモジュールは無いのでVirtualモジュールの上に作っていきましょう。

```
module KNormal = struct
```

KNormal.tの定義を行います。

```
  type t =
    | Int of int
    | Add of string * string
    | Sub of string * string
    | Print of string
    | Let of string * t * t
```

visitが変換の本体です。genidを使って新たな変数を作成しています。

```
  let rec visit (e:Syntax.t): t =
    match e with
      | Syntax.Int(i) -> Int(i)
      | Syntax.Add(aE, bE) ->
        let aId = genid("..") in
        let aK = visit(aE) in
        let bId = genid("..") in
        let bK = visit(bE) in
        Let(aId, aK, Let(bId, bK, Add(aId, bId)))
      | Syntax.Sub(aE, bE) ->
        let aId = genid("..") in
        let aK = visit(aE) in
        let bId = genid("..") in
        let bK = visit(bE) in
        Let(aId, aK, Let(bId, bK, Sub(aId, bId)))
      | Syntax.Print(aE) ->
        let aId = genid("..") in
        let aK = visit(aE) in
        Let(aId, aK, Print(aId))
      | Syntax.Let(aE, bE) ->
        Let(genid(".."), visit(aE), visit(bE))
```

applyはエントリポイントでvirtualを呼ぶだけです。

```

  let apply (e: Syntax.t): t =
    visit(e)

end
```

## Virtualモジュール

レジスタのIDを取得するregid関数を追加します。

```
  let regid = function
    | RL id -> id
    | RN id -> id
```

大きく変わるのがvisitです。Add,Sub,Printでは再帰呼び出ししません。
Letで再帰呼び出しを行っていますが、片方は末尾再帰になっていますね。
Letの連続はスタックを消費しないので大きなサイズでも問題なく処理出来ます。
１つ目のletではスタックを使いますが、これはスコープが１つ下がっているので環境を保存する為にスタックを使っているので
それほどスタックを食わないはずです。

```
  let bin env op x y =
    let r = RL(genid("..")) in
    add(Bin(r, op, M.find x env, M.find y env));
    r

  let rec visit env (k: KNormal.t): r =
    match k with
      | KNormal.Int(i) ->
        RN(string_of_int i)
      | KNormal.Add(x, y) -> bin env "add" x y
      | KNormal.Sub(x, y) -> bin env "sub" x y
      | KNormal.Let(aId, aK, bK) ->
        let aR = visit env aK in
        visit (M.add aId aR env) bK
      | KNormal.Print(aId) ->
        add(Print(M.find aId env));
        RN("void")
```

applyは引数の型が変わっていますので、変数名も変更しましょう。

```
  let apply (k: KNormal.t): t list =
    vs := [];
    let _ = visit M.empty k in
    List.rev !vs

end
```

## メイン処理

さて、処理は出来上がったので組み込んでみましょう。

```
  let vs = Virtual.apply(ast) in
```

の箇所にKNormalを追加します。

```
  let k = KNormal.apply(ast) in
  let vs = Virtual.apply(k) in
```

## test.ml

テストも修正します。

```
    let vs = Virtual.apply(src) in
```

の箇所を同じように修正します。

```
    let k = KNormal.apply(src) in
    let vs = Virtual.apply(k) in
```

テストプログラムの追加はありません。

```
$ omake test
```

と実行してテストがすべて通っていればオッケーです。次に進みましょう。
