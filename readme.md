# strollmincaml チュートリアル

このプロジェクトは、mincamlのLLVMバインディングを段階を追って作成するチュートリアルです。

良くあるプログラミング言語の作り方のように、ゼロからコンパイラを構築します。
MinCamlは約2000行でコンパイル可能な小さなコンパイラです。
このチュートリアルで少しずつ作成すれば、よりMinCamlの理解が進むでしょう。

strollmincamlは大ざっぱなMinCamlの構造を把握する事を目的としているため
今の所、最適化パスを含んでいません。また、ファイル読み込みにもまだ対応していません。
その他バグいくつかのバグが存在しています。これらの問題は今後解決する予定です。

## はじめに

strollmincamlは、文字列でLLVMのIR(LLVMのアセンブラ)ファイルを出力します。
そのため、ライブラリをインストールする必要はありません。
LLVM IRさえ分かれば、確認も*.llファイルを直接見て行えます。
ファイルを使うためコンパイルの性能はパースする処理やプロセスの起動が入る分遅くなりますが、インストール作業が楽なためこのような方法を取っています。
この手法であれば、ScalaでもF#でもPerlでもファイルを吐き出す事が出来ればコンパイルする事が出来るでしょう。
出力したllファイルは、llcコマンドでアセンブラに変換し、gccでコンパイルして、実行します。
コンパイル後直ぐ実行するので、トライアンドエラーを素早く行えます。

## システム要件

LLVMをバックエンドとして使い、OCamlでプログラミングします。
ビルドシステムはOMakeを使用します。

LLVM,OCaml,OMakeが必要ですのでgoogleで検索するなどしてインストールしてください。
開発はx86_64のOSX上で行っており、それ以外の環境では確認していません。

## 目次

- [a01/readme.md](LLVM IRとllc)
- [l01/readme.md](足し算と引き算)
- [l02/readme.md](k正規化)
- [l03/readme.md](パーサ)
- [l04/readme.md](型推論)
- [l05/readme.md](関数)
- [l06/readme.md](ファイルの分割)
- [l07/readme.md](クロージャ)
- [l08/readme.md](BoolとIf)
- [l09/readme.md](配列)
- [l10/readme.md](多値)
- [l11/readme.md](Float)
- [l12/readme.md](末尾再帰最適化)


## ライセンス

ライセンスはオリジナルのライセンスに沿ったライセンスになります。
詳しくはLICENCEファイルを参照してください。

## 参照

http://esumii.github.io/min-caml/ (オリジナルのmin-camlのWebページ)

https://github.com/nojb/llvm-min-caml (LLVMライブラリを使った実装 GC装備+小さなemitter)
https://github.com/mzp/min-caml (LLVMライブラリの実装)
https://github.com/kmacy/llmincaml

http://llvm.org/docs/LangRef.html (LLVMのリファレンス)
