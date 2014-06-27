# パーサ

この章ではパーサを作成します。

lexer.mllで字句解析を
parser.mlyでパーサを作ります。

## test.ml

以下のように、文字列から生成するように修正していきます。

```
  test("print 1;print (2 + 3);print ((2+3)-2)","(1\n5\n3\n,,0)");
```

元は構文木なので、テスト書くのが面倒ですよね。

```
  test(Let(
    Print(Int(1)),
    Let(
      Print(Add(Int(2), Int(3))),
      Print(Sub(Add(Int(2), Int(3)), Int(2))))),"(1\n5\n3\n,,0)");
```

## OMakefile

parser,lexerを設定に追加します。

```
# OCamlGeneratedFiles(parser.ml lexer.ml)
```

のコメントを外しparserとlexerを生成します。

```
OCamlGeneratedFiles(parser.ml lexer.ml)
```

utilsの後ろに、syntax,parser,lexerを追加します。
OCamlはファイル順が関係あるので注意してください。

```
  syntax
  parser
  lexer
```

## syntax.ml

syntax.mlを新規作成します。
parser.mlから読み込みたいので、main.mlからコピーして貼付け、moduleを消します。

```
type t =
  | Int of int
  | Add of t * t
  | Sub of t * t
  | Let of t * t
  | Print of t
```

## parser.mly

parser.mlyファイルを新規作成します。

ocamlyaccの形式でパーサを作ります。
字句解析ではパーサのtokenで指定した物を読み込むので、先にパーサを作成します。


プログラムの先頭の %{ と %} に囲まれた箇所に生成されたパーサに含めたいOCamlのコードを書きます。

```
%{
open Syntax
%}
```

%tokenでトークンの名前と、型を定義します。 <int> INT はint型を返しますが、それ以外は値はありません。

```
%token <int> INT
%token MINUS
%token PLUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token PRINT
%token EOF
```

演算子の優先順位を指定します。順番が下になるほど結合力は強くなります。

```
%left PLUS MINUS
%left prec_app
%left DOT
```

%typeで文法規則のリターンする型を指定します。
%startは開始する文法規則を指定します。この場合はexpを指定しています。

```
%type <Syntax.t> exp
%start exp
```

%% 以降には文法規則を記述します。

simple_expは関数呼び出しのパラメータに直接渡せる文法要素を書きます。

```
%%

simple_exp:
| LPAREN exp RPAREN
    { $2 }
| INT
    { Int($1) }
```

expには、通常の式の定義を書きます。

```
exp:
| simple_exp
    { $1 }
| exp PLUS exp
    { Add($1, $3) }
| exp MINUS exp
    { Sub($1, $3) }
| exp SEMICOLON exp
    { Let($1, $3) }
| PRINT simple_exp
    %prec prec_app
    { Print($2) }
| error
    { failwith
      (Printf.sprintf "parse error near characters %d-%d"
        (Parsing.symbol_start ())
        (Parsing.symbol_end ())) }
```

パーサはこれで完了です。

## lexer.mll

字句解析器を作成します。
lexer.mllファイルを新規作成します。


先頭の{と}の間にlexer.mlに含めたいOCamlのコードを書きます。

```
{
open Parser
}

```

正規表現を表す変数spaceとdigitを定義します。

```
let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']

```

ruleに字句解析のルールを書きます。

```
rule token = parse
| space+
    { token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| digit+
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| '-'
    { MINUS }
| '+'
    { PLUS }
| ';'
    { SEMICOLON }
| "print"
    { PRINT }
| eof
    { EOF }
| _
    { failwith
      (Printf.sprintf "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)) }
```

## main.ml

パーサと字句解析は出来ましたので、main.mlを修正していきましょう。

## Syntaxモジュール

まず、Syntaxはsyntax.mlに移動したので消します。

```
module Syntax = struct
  type t =
    | Int of int
    | Add of t * t
    | Sub of t * t
    | Let of t * t
    | Print of t
end
```

## メイン処理

ASTで書いていた所を消して、

```
let compile output ast =
  let k = KNormal.apply(ast) in
  let vs = Virtual.apply(k) in
  Emit.apply output vs

open Syntax

let _ =
  let ast =
    Let(
      Print(Int(1)),
      Let(
        Print(Add(Int(2), Int(3))),
        Print(Sub(Add(Int(2), Int(3)), Int(2))))) in
  compile "a.ll" ast;
```

以下のように修正します。

```
let parse src =
  let lexbuf = Lexing.from_string src in
  Parser.exp Lexer.token lexbuf

let compile output src =
  let ast = parse src in
  let k = KNormal.apply(ast) in
  let vs = Virtual.apply(k) in
  Emit.apply output vs

let _ =
  let src = "print 1;print (2 + 3);print ((2+3)-2)" in
  compile "a.ll" src;
```

パーサを使って文字列からコンパイルするように変更するわけです。

## test.ml

テストも文字列からテスト出来るように修正しましょう。

テストでエラーが出た場合、ソースを表示するように修正します。

```
      if !check then Printf.printf "error %s expected %s but result %s\n" src expected r else ()
```

```
    if !check then Printf.printf "test_error error %s\n" src else ()
```

```
$ omake test
```

で問題なければ完成です。
