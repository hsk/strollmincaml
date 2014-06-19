# ファイルの分割

main.mlのファイルが大分大きくなって来たので、ここでは、main.ml内のモジュールを別のファイルに分割します。

## OMakefile

mainの前に

```
  typing
  kNormal
  closure
  virtual
  emit
  main
```

と記述します。

## main.ml

```
$ touch typing.ml kNormal.ml closure.ml virtual.ml emit.ml
```
等としてファイルを新規作成します。

OCamlのモジュールは1ファイル1モジュールになるので、始めの文字を小文字にしてmlファイルを作り、
module *** = structとendを消し、ネストを１つ上げる操作を
Typing,KNormal,Closure,Virtual,Emitにそれぞれ行えば完了です。

これも面倒であれば、以下のスクリプトを実行します。
以下のスクリプトは、main.mlは書き換えませんが、モジュールをファイルに出力します。

```
<?php

$main = file_get_contents("main.ml");

$main = preg_replace_callback("/module *([^ ]+) *= *struct\n+(.*?)\n+end\n/s",function($m) {
  $filename = preg_replace_callback("/^./",function($m) {
    return strtolower($m[0]);
  },$m[1].".ml");
  $c = preg_replace("/^  /m", "", $m[2]);
  file_put_contents($filename, $c."\n");
  return "";
}, $main);

$main = preg_replace("/\n\n(\n+)/","\n\n", $main);
echo $main;
```

最後に、openだけ付け加えます。

## closure.ml

```
open Format
```

## kNormal.ml

```
open Format
open Utils
open Syntax
```

## virtual.ml

```
open Format
open Utils
```

## emit.ml

```
open Utils
```
