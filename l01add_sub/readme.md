# 足し算と引き算

## インストール

osxのbrewを使ったインストールは以下のようなコマンドでインストール出来ます。

```
brew install ocaml
brew install opam
opam install omake
```

opamとomakeのインストールには時間がかかるのでまったりやりましょう。

## はじめに

このギャップをこの章では、２パスで行ってしまいます。
１パス目で、LLVMの仮想命令列に変換し、２パス目で仮想命令列をファイルに出力します。

この文書では、このパスを増やしていき最終的には６パスまで増やします。
最適化のパスはさらに増えます。
複数パス構成は遅くなる事もありますが、なんといっても分かりやすいのが利点です。
遅くなると言っても１秒間に何ファイルもコンパイル出来るので恐ろしく重いという事はありません。

この章では、以下のシンタックスツリーを入力として、コンパイラを作ります。
パーサはl03で作成しますが、コンパイラを作るプロジェクトなので
パーサより先にLLVMを使ってしまいます。

```
  Let(
    Print(Int(1)),
    Let(
      Print(Add(Int(2), Int(3))),
      Print(Sub(Add(Int(2), Int(3)), Int(2)))))
```

## OMakefile

このプロジェクトではOMakeというOCaml製のビルドシステムを使います。
OMakeのインストールはopam等を使うと簡単に行えます。

```
$ omake --install
```

とコマンドラインで入力すると、OMakefileとOMakerootの２つのファイルが作成されます。
OMakefileがmakeのMakefileに対応するファイルなのでOMakefileを以下のように書き換えて使います。

```
.PHONY: all clean test
# OCamlGeneratedFiles(parser.ml lexer.ml)

USE_OCAMLFIND = true
OCAML_OTHER_LIBS = unix

FILES[] =
  utils
  main

PROGRAM = lllc
PROGRAM2 = test

RMS[] = *.cm* *.omc *.mli *.o .omakedb* test test.opt lexer.ml parser.ml a.*

.DEFAULT: $(OCamlProgram $(PROGRAM), $(FILES))
  ./lllc
  rm -f $(RMS)

FILES[] += test
RMS[] += $(PROGRAM) *.opt

test: $(OCamlProgram $(PROGRAM2), $(FILES))
  ./test
  rm -f $(RMS)

clean:
  rm -f $(RMS)
```

omake は

```
$ omake
```

で実行し、

```
$ omake clean
```

で不要なファイルを削除します。

```
$ omake test
```

omake testでテストを実行します。

# 実装

## utils.ml

utils.mlを作成します。
utilsは様々なユーティリティをまとめたファイルです。

asm_openはファイルを開く関数で、asm_closeでファイルを閉じる関数です。
asm_pで文字列をファイル出力し、asmはラベル用の文字列出力を行う関数です。

```
let fout = ref stdout

let asm_close () =
  close_out !fout;
  fout := stdout

let asm_open filename =
  fout := open_out filename

let asm_p x =
  output_string !fout ("  " ^ x ^ "\n")
let asm x =
  output_string !fout (x ^ "\n")

```

execはコマンドを実行し、実行した際の標準入力と標準エラーの出力と、コマンドの返り値をタプルで返します。

```
let exec cmd =
  let env = Unix.environment () in
  let cmd_out, cmd_in, cmd_err = Unix.open_process_full cmd env in
  close_out cmd_in;
  let cmd_out_descr = Unix.descr_of_in_channel cmd_out in
  let cmd_err_descr = Unix.descr_of_in_channel cmd_err in
  let selector = ref [cmd_err_descr; cmd_out_descr] in
  let errs = ref "" in
  let outs = ref "" in
  while !selector <> [] do
    let can_read, _, _ = Unix.select !selector [] [] 1.0 in
    List.iter
      (fun fh ->
         try
           if fh = cmd_err_descr
           then
             errs := !errs ^ (input_line cmd_err) ^ "\n"
           else
             outs := !outs ^ (input_line cmd_out) ^ "\n"
         with End_of_file ->
           selector := List.filter (fun fh' -> fh <> fh') !selector)
      can_read
  done;
  let code = match Unix.close_process_full (cmd_out, cmd_in, cmd_err) with
  | Unix.WEXITED(c) -> c
  | Unix.WSIGNALED(c) ->c
  | Unix.WSTOPPED(c) -> c in

  (!outs,!errs,string_of_int code)

```

genidは一意の名前を作成し返します。

```
let count = ref 0

let genid s =
  count := !count + 1;
  s ^ (string_of_int !count)

```

MとIntMapはMapを使う為のモジュールです。MはstringからIntMapはintからのMapです。

```
module M = Map.Make(String)
module IntMap = Map.Make(
  struct
    type t = int
    let compare = compare
  end
)
```

## main.ml

main.mlファイルを新規作成します。

まず、FormatとUtilsのモジュールをopenします。

```
open Format
open Utils
```

## Syntaxモジュール

Syntaxモジュールで抽象構文木を定義します。

Intが整数、Add,Subが足し算、Printが出力、Letが連続した２つの式をまとめます。
Letを使う事で複数の式をまとめて実行出来ます。

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

## Virtualモジュール

Virtualモジュールでは構文木から，LLVMの仮想命令に変換します。

```
module Virtual = struct
```

レジスタの型rとLLVMの構文を表すtを宣言します。
RLはローカル変数で、RNは即値を表します。

```
  type r =
    | RL of string
    | RN of string
```

tの要素のPrintは出力命令で、Binは２項演算子を表します。stringでaddやsubと指定する事で
複数の２項演算子を１つで表してしまいます。

```
  type t =
    | Print of r
    | Bin of r * string * r * r

```

vsは命令リストです。変換したLLVMの仮想命令はaddで追加します。

```
  let vs :t list ref = ref []

  let add (v:t): unit =
    vs := v :: !vs
```

visitが変換の本体です。再帰的に呼び出し、rを返します。

`add(Bin(reg3, "add", reg1, reg2));`などで命令列を出力しています。

```
  let rec visit(e: Syntax.t): r =
    match e with
    | Syntax.Int(i) ->
      RN(""^ string_of_int i)
    | Syntax.Add(e1, e2) ->
      let reg1 = visit(e1) in
      let reg2 = visit(e2) in
      let reg3 = RL(genid("..")) in
      add(Bin(reg3, "add", reg1, reg2));
      reg3
    | Syntax.Sub(e1, e2) ->
      let reg1 = visit(e1) in
      let reg2 = visit(e2) in
      let reg3 = RL(genid("..")) in
      add(Bin(reg3, "sub", reg1, reg2));
      reg3
    | Syntax.Let(a, b) ->
      let _ = visit(a) in
      visit(b)
    | Syntax.Print(e1) ->
      let reg1 = visit(e1) in
      let reg2 = RN("void") in
      add(Print(reg1));
      reg2
```

applyがエントリポイントで、Syntax.tを受け取り、Virtual.t listを返します。
addで追加した命令列はリストなので反転して返却します。

```
  let apply (e: Syntax.t): t list =
    vs := [];
    let _ = visit(e) in
    List.rev !vs

end
```

## Emitモジュール

EmitではLLVMの仮想命令をファイルに出力します。Virtualをよく使うのでopenします。

```
module Emit = struct

  open Virtual

```

pでレジスタを文字列に変換します。

```
  let p(r:r): string =
    match r with
      | RL(id) -> "%" ^ id
      | RN(id) -> id

```

emitが変換の本体で、asm_pで出力しています。

```
  let emit(v: t) =
    match v with
      | Bin(id, op, a, b) ->
        asm_p(p(id) ^ " = " ^ op ^ " i64 " ^ p(a) ^ ", " ^ p(b))
      | Print(a) ->
        asm_p("call i64 @print(i64 " ^ p(a) ^ ") nounwind ssp")

```

applyがエントリポイントです。ファイルを開き、メイン関数を作って、
List.iterでemitを呼び出しています。

また、補助関数のprintを実装しています。

```
  let apply(file: string) (vs: t list):unit =
    asm_open(file);

    asm("define i64 @main() nounwind ssp {");
    asm("entry:");
    List.iter (fun v -> emit(v)) vs;
    asm_p("ret i64 0");
    asm("}");

    asm("@.str = private constant [5 x i8] c\"%ld\\0A\\00\"");
    asm("define i64 @print(i64 %a) nounwind ssp {");
    asm("entry:");
    asm_p("%a_addr = alloca i64, align 8");
    asm_p("store i64 %a, i64* %a_addr");
    asm_p("%0 = load i64* %a_addr, align 8");
    asm_p("%1 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @.str, i64 0, i64 0), i64 %0) nounwind");
    asm_p("ret i64 0");
    asm("}");
    asm("declare i32 @printf(i8*, ...) nounwind");
    asm_close()

end
```

## メイン処理

print_execはコマンドを実行して結果を出力します。

```

let print_exec cmd =
  match exec cmd with
  | (a,b,c) -> fprintf std_formatter "(%s,%s,%s)@." a b c

```

ASTを直接作成し、Virtualを呼び出し、仮想命令リストに変換して、Emitでファイルに出力します。
llcでアセンブラに変換し、gccでコンパイルし、./a.outで実行します。

```
let compile output ast =
  let vs = Virtual.apply(ast) in
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
  print_exec("llc a.ll -o a.s");
  print_exec("llvm-gcc -m64 a.s");
  print_exec("./a.out")
```

## test.ml

test.mlファイルを新規作成します。
テスト用フレームワークは使わずに、独自にテストを実装しています。
チェックポイントを用意しておいて、チェックポイントを通過しないと、
次のテストが表示されないような機能があります。

```
open Main
open Utils

let count = ref 0
let ok = ref 0
let check = ref true

let test_ f src expected =
(*  try*)
    count := !count + 1;
    let r = f src in
    if r <> expected then
      if !check then Printf.printf "error expected %s but result %s\n" expected r else ()
    else
      ok := !ok + 1
(*
  with
  | Error(e) -> Printf.printf "exception %s %s\n" src e
  | _ -> Printf.printf "exception %s\n" src
*)
let test_error f src =
  try
    count := !count + 1;
    let _ = f src in
    if !check then Printf.printf "test_error error \n" else ()
  with
    _ ->
      ok := !ok + 1

let test(src, expected) =
  let f src =
    compile "a.ll" src;
    match exec("llc a.ll -o a.s") with
    | ("","","0") ->
      (match exec("llvm-gcc -m64 a.s") with
      | ("","","0") ->
        (match exec("./a.out") with
        | (a,b,c) ->
          "(" ^ a ^ "," ^ b ^ "," ^ c ^ ")"
        )
      | (a,b,c) ->
        if !check then print_string("(" ^ a ^ "," ^ b ^ "," ^ c ^ ")\n") else ();
        "llvm-gcc error"
      )
    | (a,b,c) ->
      if !check then print_string("(" ^ a ^ "," ^ b ^ "," ^ c ^ ")\n") else ();
      "llc error"
  in
    test_ f src expected

let check_point name =
  if !check then
    if !count <> !ok then (
      check := false;
      print_string("Let's try stage "^name^"...\n")
    ) else ()
  else ()

open Syntax

let _ =

  test(Let(
    Print(Int(1)),
    Let(
      Print(Add(Int(2), Int(3))),
      Print(Sub(Add(Int(2), Int(3)), Int(2))))),"(1\n5\n3\n,,0)");
  Printf.printf "test all %d ok %d ng %d\n" !count !ok (!count - !ok)
```

ファイルが出来たら実行してみましょう。

```
$ omake 
```

で、メインファイルを作成して実行出来ます。

```
*** omake: reading OMakefiles
*** omake: finished reading OMakefiles (0.01 sec)
- build . <.DEFAULT>
+ ./lllc
(,,0)
(,,0)
(1
5
3
,,0)
*** omake: done (0.17 sec, 0/2 scans, 3/9 rules, 6/93 digests)
```

以上のような結果が得られれば成功です。

```
$ omake test
```

でテストが出来ます。

```
*** omake: reading OMakefiles
*** omake: finished reading OMakefiles (0.01 sec)
- build . <test>
+ ./test
(,,0)
(,,0)
(1
5
3
,,0)
test all 1 ok 1 ng 0
*** omake: done (0.38 sec, 0/3 scans, 6/12 rules, 11/106 digests)
```

以上のような結果が得られれば成功です。
