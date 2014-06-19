# 型推論

この章では let a = 1 in a という形式の式で使える、変数を導入します。
また、型推論を導入します。

TODO: この段階はもっと分割する

## test.ml

以下のようなプログラムが動くようにします。

```
  test("let a = 1+2 in print a","(3\n,,0)");
  test("let a = let b = 1+2 in b in print a","(3\n,,0)");
  test("let a = let b = 1+2 in print b; b in print a","(3\n3\n,,0)");
```

## OMakefile

syntaxの前に、型情報を含むtypeを追加します。

```
  type
```

## syntax.ml

ASTを修正します。

```
  | Let of t * t
```

に名前と型を追加します。
また、Unit,Varを追加します。

```
  | Let of (string * Type.t) * t * t
  | Unit
  | Var of string
```

## type.ml

新規にtype.mlファイルを作成します。
型情報はUnit,Int,Varの３つの要素があります。
Unitは空のC言語のvoidで、Intはint、Varは型変数で型推論の際に使います。

```
type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Int
  | Var of t option ref
  
let gentyp () = Var(ref None) (* 新しい型変数を作る *)
```

## parser.mly

パーサもlet a = b in c形式を導入します。

パーサの先頭部分に、addtyp関数を追加します。

```
let addtyp x = (x, Type.gentyp ())
open Utils
```

トークンを追加します。

```
%token <string> IDENT
%token LET EQUAL IN 
```

優先順位を%left PLUS MINUSの上に追加します。

```
%right prec_let
%right SEMICOLON
%left EQUAL
%left PLUS MINUS
```

simple_expに変数を追加します。

```
| IDENT
    { Var($1) }
```

expにletの定義を追加します。

```
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { Let(addtyp $2, $4, $6) }
```

セミコロンのletはidを生成して用います。

```
| exp SEMICOLON exp
    { Let((genid(".."), Type.Unit), $1, $3) }
```

## lexer.mll

正規表現を作ります。

```
let lower = ['a'-'z']
let upper = ['A'-'Z']
```

トークンを追加します。IDENTはeofの手前に配置して、他のトークンより後に書いてください。

```
| "let"
    { LET }
| "in"
    { IN }
| '='
    { EQUAL }
| '_'
    { IDENT(Utils.genid("..")) }
| lower (digit|lower|upper|'_')* 
    { IDENT(Lexing.lexeme lexbuf) }
```

## main.ml


## Typing

型推論を作ります。
applyがエントリポイントで、inferが型チェック関数です。
inferの中で、型が一致する型をunifyで結びつけます。
unify内部で、まだ型が決まっていないVarがあった場合は、もう片方の型をVarの中に保存します。
保存するときには、もう片方の型に自分が含まれていないかをチェックします。
さもないと、型はループしてしまうので停止しないコンパイラになってしまいます。
deref_typeとderef_termはプログラムの最後に、型情報からVarを取り除く処理です。
型推論とは直接関係ないのですが、後続の処理でVarを意識しなくて済むようにする為に必要なのです。

```
(**
 * 型推論
 *)
module Typing = struct
  open Syntax

  exception Unify of Type.t * Type.t
  exception Error of t * Type.t * Type.t
  exception Invalid

  let extenv:Type.t M.t ref = ref M.empty

  (**
   * 型変数を消す
   *)
  let rec deref_type(x:Type.t):Type.t =
    match x with
      | Type.Var({contents=None} as r) ->
        r := Some(Type.Int);
        Type.Int
      | Type.Var({contents=Some(t)} as r) ->
        let t1 = deref_type(t) in
        r := Some(t1);
        t1
      | t -> t

  (**
   * 型変数を消す
   *)
  let rec deref_term(e:t):t =

    let deref_id_type(a:(string * Type.t)):(string * Type.t) =
      match a with
      | (x, t) -> (x, deref_type(t))
    in
    match e with
    | Add(e1, e2) -> Add(deref_term(e1), deref_term(e2))
    | Sub(e1, e2) -> Sub(deref_term(e1), deref_term(e2))
    | Let(xt, e1, e2) -> Let(deref_id_type(xt), deref_term(e1), deref_term(e2))
    | Print e -> Print(deref_term(e))
    | Unit | Var _ | Int _ -> e

  (* 単一化 *)
  let rec unify (t1:Type.t) (t2:Type.t) =

    (* 出現チェック *)
    let rec occur(r1:Type.t option ref)(r2:Type.t):bool =
      match r2 with
      | Type.Var(r2) when (r1 == r2) -> true
      | Type.Var({contents=None}) -> false
      | Type.Var({contents=Some(t2)}) -> occur r1 t2
      | _ -> false
    in

    match (t1, t2) with 
    | (Type.Unit, Type.Unit) | (Type.Int, Type.Int) -> ()
    | (Type.Var({contents=r1}), Type.Var({contents=r2})) when(r1 == r2) -> ()
    | (Type.Var({contents=Some(t1')}), _) -> unify t1' t2
    | (_, Type.Var({contents=Some(t2')})) -> unify t1 t2'
    | (Type.Var({contents=None} as r1), _) -> (* 一方が未定義の型変数の場合 *)
      if occur r1 t2 then raise(Unify(t1, t2))
      else r1 := Some(t2)
    | (_, Type.Var({contents=None} as r2)) ->
      if occur r2 t1 then raise(Unify(t1, t2))
      else r2 := Some(t1)
    | (_, _) -> raise(Unify(t1, t2))

  (**
   * 型推論
   * 式をトラバースしながら、単一化を進め型を返す．
   * 例) a + bの型は aもbもintでa+bもintなので aの型とintで単一化 bの型とintで単一化する．
   *)
  let rec infer (env:Type.t M.t) (e:t):Type.t =
    try
      match e with
        | Unit -> Type.Unit
        | Int(_) -> Type.Int
        | Add(e1, e2)
        | Sub(e1, e2) ->
          unify Type.Int (infer env e1);
          unify Type.Int (infer env e2);
          Type.Int
        | Let((x, t), e1, e2) ->
          unify t (infer env e1);
          infer (M.add x t env) e2
        | Var(x) when M.mem x env -> M.find x env
        | Var(x) -> (* 外部変数 *)
          let t = Type.Var(ref None) in
          (* println("free variable "+ x + " assumed as external "+a+"."+t)*)
          extenv := M.add x t !extenv;
          t
        | Print(x) ->
          let _ = infer env x in
          Type.Unit
    with
      | Unify(t1, t2) ->
        raise (Error(deref_term(e), deref_type(t1), deref_type(t2)))

  (* エントリポイント *)
  let apply (e:t): t =
    extenv := M.empty;

    (* 型推論 *)
    let _ = try
      unify (Type.Unit) (infer M.empty e)
    with
      | Unify(a, b) ->
        (*failwith("top level does not have type unit "+a+" "+b)*)
        failwith("top level does not have type unit")
    in

    (* 型変数を消す *)
    let deref(a:(string * Type.t)) =
      match a with
      | (x, y) -> extenv := M.add x (deref_type y) !extenv
    in
    List.iter deref (M.bindings !extenv);
    deref_term(e)

end

```

## KNormal


TODO: これ以降の文章を見直す。

type tの以下の箇所

```
    | Let of string * t * t
```

を以下のように変更します:

```
    | Let of (string * Type.t) * t * t
    | Unit
    | Var of string
```

insert_let関数を追加します:

```
  let insert_let (e, t) k = (* letを挿入する補助関数 (caml2html: knormal_insert) *)
    match e with
    | Var(x) -> k x
    | _ ->
      let x = genid("..") in
      let e', t' = k x in
      Let((x, t), e, e'), t'
```

visitにenvの引数を追加し、返り値もType.tも返すようにします:

```
  let rec visit(env:Type.t M.t)(e:Syntax.t):(t * Type.t) =
    match e with
      | Syntax.Unit -> (Unit, Type.Unit)
      | Syntax.Int(i) -> (Int(i), Type.Int)
      | Syntax.Add(e1, e2) ->
        insert_let (visit env e1) (fun x ->
          insert_let (visit env e2) (fun y ->
            (Add(x, y), Type.Int)
          )
        )
      | Syntax.Sub(e1, e2) ->
        insert_let (visit env e1) (fun x ->
          insert_let (visit env e2) (fun y ->
            (Sub(x, y), Type.Int)
          )
        )
      | Syntax.Print(aE) ->
        insert_let (visit env aE) (fun x ->
          (Print x, Type.Unit)
        )
      | Syntax.Let((x,t), e1, e2) ->
        let e1', t1 = visit env e1 in
        let e2', t2 = visit (M.add x t env) e2 in
        Let((x, t), e1', e2'), t2
      | Syntax.Var(s) ->
        Var(s), (M.find s env)
```

applyのvisitの呼び出しも修正します:

```
    fst (visit M.empty e)
```

## Virtual

type r に Type.tを追加します:

```
    | RL of Type.t * string
    | RN of Type.t * string
```

regidを変更します:

```
  let regid = function
    | RL (_,id) -> id
    | RN (_,id) -> id
```

レジスタの型を取得する関数、regtを追加します:

```
  let regt = function
    | RL (t,_) -> t
    | RN (t,_) -> t
```

binやvisitも型が付いた状態にします。

```
  let bin env op x y =
    let rx = M.find x env in
    let r = RL(regt rx, genid("..")) in
    add(Bin(r, op, rx, M.find y env));
    r
  
  let rec visit (env)(k: KNormal.t): r =
    match k with
      | KNormal.Int(i) ->
        RN(Type.Int, string_of_int i)
      | KNormal.Add(x, y) -> bin env "add" x y
      | KNormal.Sub(x, y) -> bin env "sub" x y
      | KNormal.Let((aId,aT), bK, cK) ->
        let bR = visit env bK in
        visit (M.add aId bR env) (cK)
      | KNormal.Print(aId) ->
        add(Print(M.find aId env));
        RN(Type.Unit,"void")
      | KNormal.Unit -> RN(Type.Unit, "void")
      | KNormal.Var a ->
        M.find a env

  let apply (k: KNormal.t): t list =
    vs := [];
    let _ = visit M.empty k in
    List.rev !vs
```


## Emit

pを変更

```
      | RL(_,id) -> "%" ^ id
      | RN(_,id) -> id
```

型の出力ptとrの型出力ptr,型と名前両方出力するprを追加。

```
  let pt(t:Type.t): string =
    match t with
    | Type.Int -> "i64"
    | Type.Unit -> "void"
    | Type.Var _ -> assert false

  let ptr(r:r): string =
    match r with
      | RL(t,_) -> pt t
      | RN(t,_) -> pt t

  let pr(r:r): string =
    ptr r ^ " " ^ p r
```

emitはこんなかんじ

```
  let emit(v: t) =
    match v with
      | Bin(id, op, a, b) ->
        asm_p(p(id) ^ " = " ^ op ^ " " ^ pr a ^ ", " ^ p(b))
      | Print(a) ->
        asm_p("call void @print_l(" ^ pr a ^ ") nounwind ssp")
```

## メイン

letが使えるテスト

```
  let src = "print 1;print (2 + 3);print ((2+3)-2); let a = 1+2 in print a" in
```
