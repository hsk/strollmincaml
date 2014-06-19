# LLVM IRとllc

このプロジェクトではまず、以下のプログラムをコンパイルして

```
print 1
```

以下の出力をするコンパイラを作成します。

```
1
```

このプログラムはLLVMのIRの段階のa.llでは以下のようなファイルになります。

## a.ll

a.llファイルを新規に作成して、以下の内容をコピー＆ペーストして保存してみましょう。

```
define i32 @main() nounwind ssp {
entry:
  call void @print_l(i64 1)
  ret i32 0
}
declare void @print_l(i64 %a)
```

それでは、a.llをllcを使ってアセンブラに変換してみましょう。

```
$ llc a.ll
```

うまく行っていれば、例えば以下のようなa.sファイルが出来ているはずです。

```
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
## BB#0:                                ## %entry
	pushq	%rax
	movl	$1, %edi
	callq	_print_l
	xorl	%eax, %eax
	popq	%rdx
	ret


.subsections_via_symbols
```

内容は環境に寄って変わるので、必ずこうなるとは限りません。それでも、出力されていれば成功です。

a.sを実行するには、print_l関数が必要です。

## lib.c

lib.cを新規に作成し、以下の内容を記述してください。

```
#include <stdio.h>
void print_l(long l) {
  printf("%l\n", l);
}
```

出力されたa.sとlib.cをコンパイル＆リンクするには以下のようにします:

```
$ gcc a.s lib.c
```

実行するには以下のようにします:

```
$ ./a.out
```

windowsの場合はa.exe等が出力されて以下のようにすることで実行出来るでしょう:

```
$ a.exe
```

結果、以下のように出力されていれば成功です。

```
1
```

## まとめ

この章では、a.llファイルをllcを使ってコンパイルして a.s ファイルを作成しました。
次にlib.cファイルを作成して、gccを使ってa.sとリンクして、実行ファイルを作成しました。

LLCはLLVM IRをコンパイルしてアセンブリファイルを出力します。


## 参考文献

LLVMのIRの説明は、LLVMの英語のページや、翻訳ページがありますので参照してみてください。
きつねさんの本等、書籍もいくつか出ているので参考にすると良いでしょう。
