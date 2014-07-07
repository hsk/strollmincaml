# 暗黙的型変換1

ここで暗黙的型変換を作成してみましょう。

```
  let src = "print 1;print (2 + 3);print ((2+3)-2); let a:byte = 1+2 in print a; let b=a+1 in print b" in
```

以上のようにbyteという型を指定出来るようにします。
byte型にはint型から自動的にキャストされ、int型にキャストされるとします。

## type.ml

Int型にバイト数を入れる事が出来るようにintを追加します:

```
  | Int of int
```

## syntax.ml

Int,Add,SubにもType.tを入れてbyteに対応出来るようにします:

```
  | Int of Type.t * int
  | Add of Type.t * t * t
  | Sub of Type.t * t * t
```

## parser.mly


パーサを修正します。
COLONトークンを追加します:

```
%token COLON
```

型を宣言出来るようにします:

```
| LET IDENT COLON typ EQUAL exp IN exp
    %prec prec_let
    { Let(($2,$4), $6, $8) }
```

型の文法typを追加します:

```
typ:
| IDENT {
    match $1 with
    | "int" -> Type.Int(64)
    | "byte" -> Type.Int(8)
    | _ -> assert false
}
```

intが64bitでbyteが8bitにしましょう。

## lexer.mll

字句解析に":"を追加します:

```
| ':'
    { COLON }
```

## Typingモジュール

暗黙的型変換は型推論のプログラム内で同時に行う事が出来ます。
通常の型推論のプログラムは型を取得し、letの型を決定するだけです。
したがって、関数の型は以下のようになります:

```
env -> t -> Type.t
```

しかし暗黙の型変換の場合には、キャストを式に挿入する操作が必要です。
したがって、型推論の関数の型を以下のように式と型の２つを返すようにしましょう:

```
env -> t -> t * Type.t
```

ここでinfer内のLetについての式を考えてみよう:

```
      | Let((x, t), e1, e2) ->
        let (e1, t1) = infer env e1 in
        unify t t1;
        let (e2, t2) = infer (M.add x t env) e2 in
        Let((x, t), e1, e2), t2
```

まずe1の式を型推論します。
e1の式の型はt1で、e1に変換されるとしましょう。
e1の式はxに代入されるので、型はtでもあるはずです。
e1の型はtかつt1ですのでunify t t1を呼び出します。
ここで、t1とtの型が違う場合は型エラーになります。
e2の式はe1と同様に型をチェックし、e2,t2を取得して最終的な型はt2になります。



ここで、暗黙の型変換を導入する事を考えましょう。

tとt1が違う型だった場合に、型エラーにせずに自動的にキャストを追加出来ればよい訳です。

unifyの箇所をcast関数を呼ぶように書き換えてみましょう:

```
      | Let((x, t), e1, e2) ->
        let (e1, t1) = infer env e1 in
        let e1 = cast e1 t t1 in
        let (e2, t2) = infer (M.add x t env) e2 in
        Let((x, t), e1, e2), t2
```

cast関数は内部でウマい事やって変換後のe1を返してくれる関数とします。
unifyを書き換えるcast関数を実装出来れば暗黙の型変換ができそうです。

では、castの実装をしてみましょう:

```
  let cast e t t1 =
    match deref_type t, deref_type t1 with
    | (Type.Int n, Type.Int m) when n <> m ->
      Cast(t, e)
    | _ ->
      unify t t1;
      e
```

まず、tとt1からderef_typeを使って型変数を取り除きます。
そして、２つの型がそれぞれInt型だけど、サイズが違う場合はキャスト命令を追加します。
それ以外なら、今まで通りunifyを実行するだけにします。
こうする事で、暗黙的変換を追加する事が出来ます。

同様にすべての式について実装したものが以下のプログラムになります:

```
  let rec infer (env:Type.t M.t) (e:t):(t * Type.t) =
    try
      match e with
      | Unit -> e, Type.Unit
      | Int(t, _) -> e, t
      | Add(t, e1, e2) ->
        let (e1,t1) = infer env e1 in
        let (e2,t2) = infer env e2 in
        unify t t1;
        let (e1, e2) = cast2 e1 e2 t1 t2 in
        Add(t, e1, e2), t
      | Sub(t, e1, e2) ->
        let (e1,t1) = infer env e1 in
        let (e2,t2) = infer env e2 in
        unify t t1;
        let (e1, e2) = cast2 e1 e2 t1 t2 in
        Sub(t, e1, e2), t
      | Let((x, t), e1, e2) ->
        let (e1, t1) = infer env e1 in
        let (e2, t2) = infer (M.add x t env) e2 in
        let e1 = cast e1 t t1 in
        Let((x, t), e1, e2), t2
      | Var(x) when M.mem x env -> (e, M.find x env)
      | Var(x) -> (* 外部変数 *)
        let t = Type.Var(ref None) in
        (* println("free variable "+ x + " assumed as external "+a+"."+t)*)
        extenv := M.add x t !extenv;
        e, t
      | Print(e) ->
        let (e,t) = infer env e in
        let e = cast e (Type.Int 64) t in
        Print(e), Type.Unit
      | Cast(t, e1) ->
        let (e1, t1) = infer env e1 in
        Cast(t, e1), t
    with
      | Unify(t1, t2) ->
        raise (Error(deref_term e, deref_type t1, deref_type t2))
```

Add,Subは２つの式と型を受け取るcast2関数を呼び、どちらか一方にキャストするようにします:

```
  let cast2 e1 e2 t1 t2 =
    match deref_type t1, deref_type t2 with
    | (Type.Int n, Type.Int m) when n < m ->
      Cast(t2, e1), e2
    | (Type.Int n, Type.Int m) when n > m ->
      e1, Cast(t1, e2)
    | _ ->
      unify t1 t2;
      e1, e2
```

２つの式の型を合わせる場合は、サイズが大きい方に変換するような実装になっていますね。
このようにする事で暗黙の型変換ができます。

## KNormalモジュール

KNormal.tのIntにType.tを追加します:

```
    | Int of Type.t * int
```

また、KNormal.tにCast命令を追加します:

```
    | Cast of Type.t * string
```

Int,Add,Subを書き換えます:

```
      | Syntax.Int(t, i) -> (Int(t, i), t)
      | Syntax.Add(t, e1, e2) ->
        insert_let (visit env e1) (fun x ->
          insert_let (visit env e2) (fun y ->
            (Add(x, y), t)
          )
        )
      | Syntax.Sub(t, e1, e2) ->
        insert_let (visit env e1) (fun x ->
          insert_let (visit env e2) (fun y ->
            (Sub(x, y), t)
          )
        )
```

Cast命令を追加したので処理も追加します:

```
      | Syntax.Cast(t, e1) ->
        let e1, t1 = visit env e1 in
        let x = genid("..") in
        (Let((x,t1), e1, Cast(t, x)), t)
```

## Virtualモジュール

Virtual.tにCastを追加します:

```
    | Cast of r * r
```

visitのIntの処理を書き換えます:

```
      | KNormal.Int(t, i) ->
        RN(t, string_of_int i)
```

Castの命令を追加します:

```
      | KNormal.Cast(t,x) ->
        let rx = M.find x env in
        let r = RL(t, genid("..")) in
        add(Cast(r,rx));
        r
```


## Emitモジュール

型を文字列化するpt関数のIntの処理を書き換えます:

```
    | Type.Int n -> "i" ^ string_of_int n
```

emitにCastを追加し以下のようにサイズによってsext trunc bitcastを呼ぶようにします:

```
      | Cast(r1,r2) ->
        (match (regt r1, regt r2) with
        | Type.Int(n), Type.Int(m) when n > m ->
          asm_p(p r1 ^" = sext "^pr r2^" to "^ ptr r1)
        | Type.Int(n), Type.Int(m) when n < m ->
          asm_p(p r1 ^" = trunc "^pr r2^" to "^ ptr r1)
        | _ ->
          asm_p(p r1 ^" = bitcast "^pr r2^" to "^ ptr r1)
        )
```

sextは型を大きくし、truncは小さくし、bitcastは同じサイズの型を置き換えます。
LLVMではキャストの内容によって命令が変わるのでここで吸収している訳です。


以上で作業は終わりです。omake omake testをして問題なければ完成です。

