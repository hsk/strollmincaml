# OCaml入門

とっても簡単なOCaml入門をこの章で行います。
簡単すぎるので、より詳しく知りたい人は入門書等を買って読むと良いでしょう。

インストール

```
brew install ocaml
brew install opam
opam install omake
```
opamとomakeのインストールには時間がかかるのでまったりやりましょう。

## hello01.ml

まずは、hello worldを作ります。

hello01.mlを新規作成して以下の無いようにします:

```
let _ =
  print_string "hello world!\n"
```

では、インタプリタで実行してみましょう。

```
$ ocaml hello01.ml
```

さて、以下のように表示されれば成功です。

```
hello world!
```

## コンパイルする

```
$ ocamlc hello01.ml
```

```
$ a.out
```

以下のように表示されれば成功です。

```
hello world!
```


## ocamlopt

ocamlの最適化コンパイラを使う場合はocamloptを使います。


```
$ ocamlc hello01.ml
```

```
$ a.out
```

以下のように表示されれば成功です。

```
hello world!
```

ocamlcもocamloptも-oで出力ファイル名を指定する事も出来ます。
