# C言語風構文で書き換え可能な変数

## test.ml

以下のように、a #= 1として変数を初期化すると、書き換え可能な変数になるようにします。

```
  test("{ a #= 1 print(a) a=2 print(a) }","(1\n2\n,,0)");
```

つまり以下のプログラムに内部的に変換されます:

```
  test("{ a := Array.create(1,1) print(a[0]) a[0]=2 print(a[0]) }","(1\n2\n,,0)");
```

さらに追加する諸々のテストです:

```
  test("{t:=new 10 print(*t) *t=2 print(*t)}","(10\n2\n,,0)");


  test("{a:=1 print(a)}", "(1\n,,0)");
  test("{a:=1 b:=2 print(a+b)}", "(3\n,,0)");

  check_point "immutable value";


  test("{a:= new 1; print(*a)}", "(1\n,,0)");
  test("{a:= new 1; *a=2; print(*a)}", "(2\n,,0)");

  check_point "ptr";

  (* bにポインタコピー *)
  test("{a:= new 1; b:=a; print(*b)}", "(1\n,,0)");
  test("{a:= new 1; b:=a; *a=2; print(*b)}", "(2\n,,0)");
  test("{a:= new 1; b:=a; *b=2; print(*a)}", "(2\n,,0)");

  (* bの入れ物を作成 *)
  test("{a:= new 1; b:=new *a; print(*b)}", "(1\n,,0)");
  test("{a:= new 1; b:=new *a;*a=2; print(*b)}", "(1\n,,0)");
  test("{a:= new 1; b:=new *a;*b=2; print(*b)}", "(2\n,,0)");
  test("{a:= new 1; b:=new *a;*b=2; print(*a)}", "(1\n,,0)");

  check_point "ptr ptr";

  test("{a:= new 1; *a=5; b:=*a; print(b)}", "(5\n,,0)");
  test("{a:= new 1; b:=2; *a=b; print(*a)}", "(2\n,,0)");

  check_point "ptr imm";

(* aがmutableなら aは*aにする *)
(* a#=nは a:=new nである *)
  test("{a#= 1; print(a)}", "(1\n,,0)");
  test("{a#= 1; a=2; print(a)}", "(2\n,,0)");

  check_point "ref";

  (* 値を取り出し *)
  test("{a#= 1; a=5; b:=a; print(b)}", "(5\n,,0)");

  test("{a#= 1; b:=a; a=5; print(b)}", "(1\n,,0)");
  test("{a#= 1; b:=2; a=b; print(a)}", "(2\n,,0)");

  check_point "ref imm";

  (* bにポインタコピー *)

  test("{a#= 1; b:= & a; print(*b)}", "(1\n,,0)");
  test("{a#= 1; b:= & a; a=2; print(*b)}", "(2\n,,0)");
  test("{a#= 1; b:= & a; *b=2; print(a)}", "(2\n,,0)");

  (* bの入れ物を作成 *)

  test("{a#= 1; b:=new a; print(*b)}", "(1\n,,0)");
  test("{a#= 1; b:=new a;a=2; print(*b)}", "(1\n,,0)");
  test("{a#= 1; b:=new a;*b=2; print(*b)}", "(2\n,,0)");
  test("{a#= 1; b:=new a;*b=2; print(a)}", "(1\n,,0)");

  check_point "ref ptr";

  (* bにポインタコピー C++の参照 *)
(*
  test("{a#= 1; b#&long= a; b}", "(1\n,,0)");
  test("{a#= 1; b#&long= a; a=2; b}", "(2\n,,0)");
  test("{a#= 1; b#&long= a; b=2; a}", "(2\n,,0)");
*)
  (* bの入れ物を作成 *)

  test("{a#= 1; b#=a; print(b)}", "(1\n,,0)");
  test("{a#= 1; b#=a;a=2; print(b)}", "(1\n,,0)");
  test("{a#= 1; b#=a;b=2; print(b)}", "(2\n,,0)");
  test("{a#= 1; b#=a;b=2; print(a)}", "(1\n,,0)");

  check_point "ref ref";

  test("{a:=new 1; b:= new a; print(*a)}", "(1\n,,0)");

  test("{a:=new 1; b:= new a; *a=5; print(**b)}", "(5\n,,0)");
  test("{a#=1; b#= & a; a=5; print(*b)}", "(5\n,,0)");
  test("{a#=1; b#= & a; *b=5; print(a)}", "(5\n,,0)");


  check_point "ref ref";
```

## OMakefile

syntaxの手前にastを追加します:

```
  ast
  syntax
```


## parser.mly

```
open Syntax
let addtyp x = (x, Type.gentyp ())
```

の箇所をAstに書き換えます:

```
open Ast
let addtyp x = (x, Type.gentyp ())
let addtypf x = (x, Type.gentyp (), false)
let addtypt x = (x, Type.gentyp (), true)
```

トークンを追加します:

```
%token VALEQ VAREQ AMP
```

exp_prim

```
| LBRACE exps RBRACE { Let(addtypf(Utils.genid("..")),Unit, $2) }
```

exps

```
  | a -> Let(addtypf(Utils.genid("..")), a, $2)
```

exp_post

```
| exp_post LPAREN RPAREN VALEQ exp {
  match $1 with
  | Var(a) ->
    LetRec({name=addtypf a;args=[];body=$5}, Unit)
  | _ -> assert false
}
```

exp_val

```
    Let(addtypf a, $3, Unit)
```

exp_valに以下を追加します:

```
| exp_post VAREQ exp_eq {
  match $1 with
  | Var(a) ->
    Let(addtypt a, $3, Unit)
  | Tuple(ls) ->
    let ls = List.fold_right
      (fun t ls ->
        match t with
        | Var(a) -> (addtyp a) :: ls
        | Unit -> ls
        | _ -> 
          Format.fprintf Format.std_formatter "t=%a@." print_t t;
          assert false
      )
      ls
      []
    in
    LetTuple(ls, $3, Unit)
  | t -> 
    Format.fprintf Format.std_formatter "t=%a@." print_t t;
    assert false
}
```

## lexer.mll

以下のルールを追加します:

```
| "#="           { VAREQ }
```

## ast.ml

Syntax.mlをコピーしてast.mlを作成します。

type tのletを書き換えます:

```
  | Let of (string * Type.t * bool) * t * t
```

fundefも書き換えます:

```
and fundef = { name : string * Type.t * bool; args : (string * Type.t) list; body : t }
```

print_tのLetを書き換えます:

```
  | Let((s,t,m),a,b) -> fprintf ppf "Let((\"%s\",%a,%b),%a,%a)" s Type.print_t t m print_t a print_t b
```

print_tのLetRecを書き換えます:

```
  | LetRec({name=(s,t,m);args=args;body=e1},e2) ->
    fprintf ppf "LetRec({name=(\"%s\",%a,%b);args=%a;body=%a},%a)"
      s Type.print_t t m print_sts args print_t e1 print_t e2
```

## syntax.ml

Ast.tからSyntax.tへ変換する関数visitを追加します:

```
let rec visit(env:S.t) (e: Ast.t):t =
  match e with
  | Ast.Let((id,t, true), e1, e2) ->
    (* a#long=1 b#long=a は 変形しないようにするため、Refを後で付ける　*)
    Let((id,Type.Array t), 
      Array(Int 1, visit env e1),
      visit (S.add id env) e2
        (* ムータブルな変数集合に加える *)
    )
  | Ast.Let((id,t, false), e1, e2) ->
    Let((id,t),
      visit env e1,
      visit
        (* ムータブルな変数集合から消す *)
        (S.remove id env) e2
    )
  | Ast.LetRec({Ast.name=(id,t,m);Ast.args=args;Ast.body=body},e)->

    LetRec({
      name=(id,t);
      args=args;
      body=visit env body
      },
      visit env e
    )

  (* ムータブルな変数は中身を取り出す *)
  | Ast.Var(id) when S.mem id env -> Get(Var(id),Int 0)
  (* ムータブルな変数のリファレンスはそのままのポインタ値　*)
  (*  | Ast.Ref(Ast.Var(id)) when S.mem id env -> Var(id) *)
  | Ast.Array (Ast.Var(id), Ast.Int 0) when S.mem id env ->
    Var(id)

  | Ast.Var(id) -> Var(id)

  | Ast.Put(e1, e2, e3) -> Put(visit env e1, visit env e2, visit env e3)
  | Ast.Unit -> Unit
  | Ast.Int(i) -> Int i
  (* binnary operators *)
  | Ast.Add (e1, e2) -> Add(visit env e1, visit env e2)
  | Ast.Sub (e1, e2) -> Sub(visit env e1, visit env e2)
  | Ast.FAdd (e1, e2) -> FAdd(visit env e1, visit env e2)
  | Ast.FSub (e1, e2) -> FSub(visit env e1, visit env e2)
  | Ast.FMul (e1, e2) -> FMul(visit env e1, visit env e2)
  | Ast.FDiv (e1, e2) -> FDiv(visit env e1, visit env e2)
  | Ast.Eq (e1, e2) -> Eq(visit env e1, visit env e2)
  | Ast.LE (e1, e2) -> LE(visit env e1, visit env e2)

  | Ast.Neg e -> Neg(visit env e)
  | Ast.FNeg e -> FNeg(visit env e)
  | Ast.Float e -> Float(e)
  | Ast.Bool e -> Bool(e)
  | Ast.Not e -> Not(visit env e)
  | Ast.If (e1, e2, e3) -> If(visit env e1, visit env e2, visit env e3)
  | Ast.Array (e1, e2) -> Array(visit env e1, visit env e2)
  | Ast.Get (e1, e2) -> Get(visit env e1, visit env e2)
  | Ast.Tuple es -> Tuple(List.map (visit env) es)
  | Ast.LetTuple (ns, e1, e2) -> LetTuple(ns, visit env e1, visit env e2)
  | Ast.App(e1, es) -> App(visit env e1, List.map (visit env) es)
```

applyを追加します:

```
let apply (e: Ast.t):t =
  visit S.empty e
```

## main.ml

compile関数に以下を追加します:

```
  fprintf std_formatter "ast=%a@." Ast.print_t ast;
  let ast = Syntax.apply(ast) in
```
