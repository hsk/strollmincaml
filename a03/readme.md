# インタプリタ

intp.mlというファイルを新規作成し以下の内容を記述します。

```
type t =
| Int of int
| Add of t * t
| Sub of t * t

let rec eval t =
  match t with
  | Int i -> i
  | Add(x,y) -> (eval x) + (eval y)
  | Sub(x,y) -> (eval x) - (eval y)

let _ =
  let ast = Sub(Add(Int(1),Int(2)),Int(1)) in
  let a = eval ast in
  print_string ((string_of_int a) ^ "\n")

```

実行してみましょう。

```
$ ocaml intp.ml
2
$ _
```

な、何がおこった？

ocamlでは、enumの凄いバージョンをtypeを使って作る事が出来ます。

```
type t =
| Int of int
| Add of t * t
| Sub of t * t
```

こうすることで、Int,Add,Subという3つの要素を持っていて、さらに内部にデータを持っているデータを作成出来るのです。

そして、以下のように書いて1 + 2 - 1を表す構文木を作成出来ます。

```
let ast = Sub(Add(Int(1),Int(2)),Int(1)) in
```

ocamlではlet rec a b = b + 1のようにすることで関数を作成出来ます。

```
let rec eval t =
  match t with
  | Int i -> i
  | Add(x,y) -> (eval x) + (eval y)
  | Sub(x,y) -> (eval x) - (eval y)
```

したがってこれはevalという関数です。

match withはswitch文の凄い奴で、type tで宣言した物に付いてもswitch出来ます。
さらに、バインディング機能といって、内部のデータを変数に割り当てる事が出来ます。

eval評価器はIntが来たら内部の値をiにバインドしてiを返します。
Addが来たら、内部の値、xとyにバインドして、evalに渡して結果を受け取って足して返します。
Subも同様です。再帰呼び出しすることで、うまく計算出来ています。


```
let _ =
  let ast = Sub(Add(Int(1),Int(2)),Int(1)) in
  let a = eval ast in
  print_string ((string_of_int a) ^ "\n")
```

aはintなので、string_of_intで文字列に変換し、
実行している訳です。


こんな短くJavaやCでは書けないよね。関数型言語凄いんです。
