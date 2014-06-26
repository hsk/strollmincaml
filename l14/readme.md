# C言語風構文でポインタ

この章では、配列を使ってポインタ変数を扱えるようにします。

## test.ml

```
test("{t:=new 10 print(*t) *t=2 print(*t)}","(10\n2\n,,0)");
```

は以下の意味と同じ意味を持たせます。

```
test("{t:=Array.create(1,10) print(t[0]) t[0]=2 print(t[0])}","(10\n2\n,,0)");
```

## syntax.ml

NEWトークンを追加します:

```
%token NEW MUL
```

exp_unaryに以下を加えます:

```
| NEW exp_unary { Array(Int 1, $2) }
| MUL exp_unary { Get($2, Int 0) }
```

exp_valに以下を追加します:

```
| MUL exp_unary ASSIGN exp_eq { Put($2, Int 0, $4) }
```

## lexer.mll

以下の項目を加えます:

```
| "new" { NEW }
| '*' { MUL }
```
