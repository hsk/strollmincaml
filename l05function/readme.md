# 関数

この章では関数を追加します。関数を追加する為に，後にクロージャを使えるようにする事も考え
Closureモジュールを作成して、型だけ合わせておきます。
次の章では、main.ml内のモジュールを外に移動するので、この章のmain.mlが最適化パスを抜いた全体像になります。

## test.ml

以下のような処理が出来るようにします。テストに加えましょう。

```
  test("
    let rec f x =
      x+1
    in print (f 1);
    print (2 + 3);
    print ((2+3)-2);
    let a = 1+2 in
    print a
  ", "(2\n5\n3\n3\n,,0)");
```

コンパイル時にソースを表示するようにします:

```
let test(src, expected) =
  Printf.printf "compile %s\n" src;
```

## OMakefile

mainの手前にStringのSetとMapのモジュールをs,mを追加します。

```
  s
  m
  main
```

## s.ml

StringのSetのモジュールを作成します。
s.mlファイルを新規作成して以下のソースを書きます:

```
(* customized version of Set *)

module S =
  Set.Make
    (struct
      type t = string
      let compare = compare
    end)
include S

let of_list l = List.fold_left (fun s e -> add e s) empty l
```

## m.ml

m.mlファイルを新規作成します。
今までは、Utils内のMを使っていましたが今後は、add_list等の追加機能があるこちらを使います。

```
(* customized version of Map *)

module M =
  Map.Make
    (struct
      type t = string
      let compare = compare
    end)
include M

let add_list xys env = List.fold_left (fun env (x, y) -> add x y env) env xys
let add_list2 xs ys env = List.fold_left2 (fun env x y -> add x y env) env xs ys
```

## syntax.ml

type tに関数と継続の式を表すLetRecと関数適用を表すAppと関数定義のfundefを追加します:

```
  | LetRec of fundef * t
  | App of t * t list
and fundef = { name : string * Type.t; args : (string * Type.t) list; body : t }
```

この際に、Printを消します:

```
  | Print of t
```

汎用的な関数呼び出しAppを使えるので、Printは要らなくなる訳です。

デバッグ用にprint関数を追加します。

```
open Format

let rec print_t ppf = function
  | Int(i) -> fprintf ppf "Int(%d)@?" i
  | Add(a,b) -> fprintf ppf "Add(%a,%a)@?" print_t a print_t b
  | Sub(a,b) -> fprintf ppf "Sub(%a,%a)@?" print_t a print_t b
  | Let((s,t),a,b) -> fprintf ppf "Let((\"%s\",%a),%a,%a)" s Type.print_t t print_t a print_t b
  | Unit -> fprintf ppf "Unit@?"
  | Var(a) -> fprintf ppf "Var(\"%s\")@?" a
  | LetRec({name=(s,t);args=args;body=e1},e2) ->
    fprintf ppf "LetRec({name=(\"%s\",%a);args=%a;body=%a},%a)"
      s Type.print_t t print_sts args print_t e1 print_t e2
  | App(t,ts) -> fprintf ppf "App(%a,%a)" print_t t print_ts ts
and print_st ppf = function
  | (s,t) -> fprintf ppf "(\"%s\",%a)" s Type.print_t t
and print_sts ppf ls = Type.prints print_st ppf ls
and print_ts ppf ls = Type.prints print_t ppf ls
```

## type.ml

type tに関数の型を追加します。多値はこの章では使いませんが、クロージャを追加するときに使うのでまとめて追加してしまいます:

```
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
```

デバッグ用のprints print_tを追加します。

```
open Format

let rec prints f ppf ls =
  let rec loop ppf = function
    | [] -> ()
    | [l] -> f ppf l
    | l::ls -> fprintf ppf "%a; %a@?" f l loop ls
  in fprintf ppf "[%a]@?" loop ls

let rec print_t ppf = function
  | Unit -> fprintf ppf "Unit@?"
  | Int -> fprintf ppf "Int@?"
  | Var({contents=None}) -> fprintf ppf "Var(ref None)@?"
  | Var({contents=Some t}) -> fprintf ppf "Var(ref (Some(%a)))@?" print_t t
  | Fun(ts,t) -> fprintf ppf "Fun(%a,%a)@?" print_ts ts print_t t
  | Tuple(ts) -> fprintf ppf "Tuple(%a)@?" print_ts ts
and print_ts ppf ts = prints print_t ppf ts
```

## utils.ml

utilsから、module Mを削除します。
```
module M = Map.Make(String)
```

## parser.mly

RECトークンを追加します:

```
%token REC
```

PRINTトークンを消します:

```
%token PRINT
```

simple_expにUnitの構文を追加します:

```
| LPAREN RPAREN
    { Unit }
```

expからPRINTの項目を消して、

```
| PRINT simple_exp
    %prec prec_app
    { Print($2) }
```

以下を追加します:

```
| LET REC fundef IN exp
    %prec prec_let
    { LetRec($3, $5) }
| exp actual_args
    %prec prec_app
    { App($1, $2) }
```

以下の構文要素を追加します:

```
fundef:
| IDENT formal_args EQUAL exp
    { { name = addtyp $1; args = $2; body = $4 } }

formal_args:
| IDENT formal_args
    { addtyp $1 :: $2 }
| IDENT
    { [addtyp $1] }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }
```

## lexer.mll

字句解析のルールからprintを消して、

```
| "print"
    { PRINT }
```

recをletの下に追加します:

```
| "rec"
    { REC }
```

## main.ml

## Typingモジュール

deref_typeに以下を追加します:

```
      | Type.Fun(t1s, t2) ->
        Type.Fun(List.map deref_type t1s, deref_type t2)
```

deref_termのPrintの箇所を

```
    | Print(e) -> Print(deref_term(e))
```

以下に書き換えます:

```
    | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
        LetRec({ name = deref_id_type xt;
           args = List.map deref_id_type yts;
           body = deref_term e1 },
         deref_term e2)
    | App(e, es) -> App(deref_term e, List.map deref_term es)
```

occurに以下を追加します:

```
      | Type.Fun(t2s, t2) -> List.exists (occur r1) t2s || occur r1 t2
```

unifyに以下を追加します:

```
    | Type.Fun(t1s, t1'), Type.Fun(t2s, t2') ->
        (try
          List.iter2 unify t1s t2s
        with
          | Invalid_argument("List.iter2") -> raise (Unify(t1, t2))
        );
        unify t1' t2'
```

inferのPrintの箇所を

```
        | Print(x) ->
          let _ = infer env x in
          Type.Unit
```

以下に書き換えます:

```
        | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
          let env = M.add x t env in
          unify t (Type.Fun(List.map snd yts, infer (M.add_list yts env) e1));
          infer env e2
        | App(e, es) ->
          let t = Type.Var(ref None) in
          unify (infer env e) (Type.Fun(List.map (infer env) es, t));
          t
```

## KNormalモジュール

type tのPrintを消します:

```
    | Print of string
```

type tに以下を追加します:

```
    | LetRec of fundef * t
    | App of string * string list
    | ExtFunApp of string * string list * Type.t
  and fundef = {
    name : string * Type.t;
    args : (string * Type.t) list;
    body : t }
```

visitからPrintを消して、

```
      | Syntax.Print(aE) ->
        insert_let (visit env aE) (fun x ->
          (Print x, Type.Unit)
        )
```

visitに以下を追加します:

```
      | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2) ->
        let env' = M.add x t env in
        let e2', t2 = visit env' e2 in
        let e1', t1 = visit (M.add_list yts env') e1 in
        LetRec({ name = (x, t); args = yts; body = e1' }, e2'), t2
      | Syntax.App(Syntax.Var(f), e2s) when not (M.mem f env) ->
        (* 外部関数の呼び出し (caml2html: knormal_extfunapp) *)
          (match M.find f !Typing.extenv with
          | Type.Fun(_, t) ->
            let rec bind xs = function
              (* "xs" are identifiers for the arguments *)
              | [] -> ExtFunApp(f, xs, t), t
              | e2 :: e2s ->
                insert_let (visit env e2)
                  (fun x -> bind (xs @ [x]) e2s) in
            bind [] e2s (* left-to-right evaluation *)
          | _ -> assert false)
      | Syntax.App(e1, e2s) ->
        (match visit env e1 with
        | _, Type.Fun(_, t) as g_e1 ->
          insert_let g_e1 (fun f ->
            let rec bind xs = function (* "xs" are identifiers for the arguments *)
            | [] -> App(f, xs), t
            | e2 :: e2s ->
              insert_let (visit env e2)
                (fun x -> bind (xs @ [x]) e2s) in
                bind [] e2s
          ) (* left-to-right evaluation *)
        | _, t ->
          fprintf str_formatter "type error in app %a" Type.print_t t;
          failwith (flush_str_formatter())
        )
```

## Closureモジュール

新しく、Closureモジュールを作成します。
Closureモジュールの一番の目的はクロージャの変換ですが、ここではクロージャの実装は置いておいて、
関数がクロージャ変換でかき変わる様子だけを見る事が出来ます。

```
module Closure = struct

  type closure = { entry : string; actual_fv : string list }
  type t = (* クロージャ変換後の式 (caml2html: closure_t) *)
    | Unit
    | Int of int
    | Add of string * string
    | Sub of string * string
    | Let of (string * Type.t) * t * t
    | Var of string
    | AppDir of string * string list
    | ExtFunApp of string * string list * Type.t
  type fundef = {
    name : string * Type.t;
    args : (string * Type.t) list;
    body : t
  }
  type prog = Prog of fundef list * t

  let toplevel: fundef list ref = ref []

  (**
   * クロージャ変換ルーチン本体
   * 基本的にはKからCへの変換をする．
   *)
  let rec visit (e:KNormal.t):t =
    match e with
    | KNormal.Unit -> Unit
    | KNormal.Int(i) -> Int(i)
    | KNormal.Add(x, y) -> Add(x, y)
    | KNormal.Sub(x, y) -> Sub(x, y)
    | KNormal.Var(x) -> Var(x)
    | KNormal.Let((x, t), e1, e2) -> Let((x, t), visit e1, visit e2)
    | KNormal.LetRec({KNormal.name=(x, t);KNormal.args=yts;KNormal.body=e1}, e2) ->
      toplevel := {name=(x, t); args=yts; body=visit e1 } :: !toplevel;
      visit e2
    | KNormal.App(x, ys) -> AppDir(x, ys)
    | KNormal.ExtFunApp(x, ys, t) -> ExtFunApp(x, ys, t)

  (**
   * クロージャ変換
   *)
  let apply(e:KNormal.t): prog =
    toplevel := [];
    let e = visit e in
    Prog(List.rev !toplevel, e)

end

```

## Virtualモジュール

type rにグローバルな変数名を表すRGを追加します:

```
    | RG of Type.t * string
```

type tのPrintを消して、

```
    | Print of r
```

汎用的なCallに書き換えます:

```
    | Call of r * r * r list
```

type tにはRetも追加し、fundefとprogも追加します:

```
    | Ret of r

  type fundef =
     {name : string; args : (string * Type.t) list; body : t list; ret : Type.t}

  type prog = Prog of fundef list
```

regidに以下を追加します:

```
    | RG (_,id) -> id
```

regtに以下を追加します:

```
    | RG (t,_) -> t
```

visitのKNormal.はClosureに変更し、Printは消します:

```
  let rec visit (env)(k: KNormal.t): r =
    match k with
      | KNormal.Int(i) ->
      	:
      | KNormal.Add(x, y) -> bin env Type.Int "add" x y
      | KNormal.Sub(x, y) -> bin env Type.Int "sub" x y
      | KNormal.Let((aId,aT), bK, cK) ->
      	:
      | KNormal.Print(aId) ->
      	:
      | KNormal.Unit -> RN(Type.Unit, "0")
      | KNormal.Var a ->
      	:
```

を

```
  let rec visit(env:r M.t)(c: Closure.t): r =
    match c with
      | Closure.Int(i) ->
      	:
      | Closure.Add(x, y) -> bin env Type.Int "add" x y
      | Closure.Sub(x, y) -> bin env Type.Int "sub" x y
      | Closure.Let((aId,aT), bK, cK) ->
      	:
      | Closure.Unit -> RN(Type.Unit, "0")
      | Closure.Var a ->
      	:
```

に変更します。


visitに関数適用の処理を追加します:

```
      | Closure.AppDir(nameId, prmIds) ->
        let prmRs = List.map (fun prmId -> M.find prmId env ) prmIds in
        let nameR = M.find nameId env in
        let retR = RL(regt nameR, genid("..")) in
        add(Call(retR, nameR, prmRs));
        retR
      | Closure.ExtFunApp(nameId, prmIds, t) ->
        let prmRs = List.map (fun prmId -> M.find prmId env) prmIds in
        let nameR = RG(t, nameId) in
        let retR = RL(t, genid("..")) in
        add(Call(retR, nameR, prmRs));
        retR
```

applyはvisitfunを加えfold_leftでループして処理します:

```
  let visitfun env {
      Closure.name = (x, t); 
      Closure.args = yts;
      Closure.body = e } =

    match t with
    | Type.Fun(_, t) ->
      vs := [];
      let env = M.add x (RG(t,x)) env in
      let env' = M.add_list (List.map (fun (s,t) -> (s, RL(t,s))) yts) env in
      let r = visit env' e in
      add(Ret(r));
      (env, { name = x; args = yts; body = List.rev !vs; ret = t })
    | _ -> assert false

  let apply (Closure.Prog(fundefs, e)): prog =
    let fundefs = fundefs @ [{Closure.name=("main", Type.Fun([], Type.Unit));
      Closure.args=[]; Closure.body= e}] in
    let (_,fundefs) =
      List.fold_left
        (fun (env, fundefs) fundef ->
          let (env, fundef) = visitfun env fundef in
          (env, fundef::fundefs)
        )
        (M.empty, [])
        fundefs
    in
    Prog(fundefs)
```

## Emitモジュール

pにRGを追加します:

```
      | RG(_,id) -> "@" ^ id
```

ptはrecを付けて

```
  let rec pt(t:Type.t): string =
```

Fun,Tupleを追加します。
main関数のリターン値を一定にしたいのでUnitはi64にしてしまいます:

```
    | Type.Unit -> "i64"
    | Type.Fun(ts,t) -> pt t ^ "(" ^ String.concat ", " (List.map pt ts) ^ ")*"
    | Type.Tuple(ts) -> "{" ^ String.concat ", " (List.map pt ts) ^ "}"
```

prt にRGを追加します:

```
      | RG(t,_) -> pt t
```

emitからもPrintを削除し、
```
      | Print(a) ->
        asm_p("call i64 @print(" ^ pr a ^ ") nounwind ssp")
```

emitにCallとRetを追加します:

```
      | Call(id, r, prms) ->
        let ps = String.concat ", " (List.map pr prms) in
        (match regt id with
          | Type.Unit ->
            asm_p("call " ^ pr(r) ^ "(" ^ ps ^ ") nounwind ssp")
          | _ ->
            asm_p(p(id) ^ " = call " ^ pr(r) ^ "(" ^ ps ^ ") nounwind ssp")
        )
      | Ret(a) ->
        (match regt a with
          | Type.Unit ->
            asm_p("ret i64 0")
          | _ ->
            asm_p("ret " ^ pr(a))
        )
```

applyの受け取りをProgを受け取るように修正します:

```
  let apply(file: string) (Prog(fundefs)):unit =
```

メイン関数だった所を消して、

```
    asm("define i64 @main() nounwind ssp {");
    asm("entry:");
    List.iter (fun v -> emit(v)) vs;
    asm_p("ret i64 0");
    asm("}");
```

関数をループして出力するようにします:

```
    List.iter (fun
      {name = x; args = args; body = vs; ret = t} ->
      let args = String.concat ", " (List.map (fun (s,t)-> pt t ^ " %" ^ s) args) in
      asm("define "^pt t^" @"^x^"("^args^") nounwind ssp {");
      asm("entry:");
      List.iter (fun v -> emit(v)) vs;
      asm("}");

    ) fundefs;

```

## メインの処理

テストを以下に書き換えます:

```
  let src = "let rec f x = x+1 in print (f 1); print (2 + 3);print ((2+3)-2); let a = 1+2 in print a" in
```

コンパイル部分は

```
  let k = KNormal.apply(ast) in
  let vs = Virtual.apply(k) in
  Emit.apply "a.ll" vs;
```

を

```
  fprintf std_formatter "ast=%a@." Syntax.print_t ast;
  let ast = Typing.apply(ast) in
  fprintf std_formatter "ast=%a@." Syntax.print_t ast;
  let k = KNormal.apply(ast) in
  fprintf std_formatter "knormal ok@.";
  let c = Closure.apply(k) in
  fprintf std_formatter "closure ok@.";
  let v = Virtual.apply(c) in
  fprintf std_formatter "virtual ok@.";
  Emit.apply output v;
  fprintf std_formatter "emit ok@."
```

と変更して途中結果を表示するようにします。

omake omake testで問題なければ完了です。

