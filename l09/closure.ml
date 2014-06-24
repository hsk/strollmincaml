open Format

type closure = { entry : string; actual_fv : string list }
type t = (* クロージャ変換後の式 (caml2html: closure_t) *)
  | Unit
  | Int of int
  | Add of string * string
  | Sub of string * string
  | Let of (string * Type.t) * t * t
  | Var of string
  | MakeCls of (string * Type.t) * closure * t
  | AppCls of string * string list
  | AppDir of string * string list
  | ExtFunApp of string * string list * Type.t
  | Bool of bool
  | If of string * t * t
  | LE of string * string
  | Eq of string * string
  | Get of string * string
  | Put of string * string * string
  | ExtArray of string * Type.t

type fundef = {
  name : string * Type.t;
  args : (string * Type.t) list;
  formal_fv : (string * Type.t) list;
  body : t
}
type prog = Prog of fundef list * t

let rec print_t ppf = function
  | Int i -> fprintf ppf "Int(%d)@?" i
  | Add(a,b) -> fprintf ppf "Add(\"%s\",\"%s\")@?" a b
  | Sub(a,b) -> fprintf ppf "Sub(\"%s\",\"%s\")@?" a b
  | Let((s,t),a,b) -> fprintf ppf "Let((\"%s\",%a),%a,%a)@?" s Type.print_t t print_t a print_t b
  | Unit -> fprintf ppf "Unit@?"
  | Var(a) -> fprintf ppf "Var(\"%s\")@?" a
  | MakeCls((s,t),{entry=ent;actual_fv=ss},b) ->
    fprintf ppf "MakeCls((\"%s\",%a),{entry=\"%s\";actual_fv=[%s]},%a)@?"
      s Type.print_t t
      ent
      (String.concat "; " ss) 
      print_t b
  | AppCls(s,ss) -> fprintf ppf "AppCls(\"%s\",[%s])@?" s (String.concat "; " ss)
  | AppDir(s,ss) -> fprintf ppf "AppDir(\"%s\",[%s])@?" s (String.concat "; " ss)
  | ExtFunApp(s,ss,t) -> fprintf ppf "ExtFunApp(\"%s\",[%s],%a)@?" s (String.concat "; " ss) Type.print_t t
  | Bool(b) -> fprintf ppf "Bool(%b)@?" b
  | If(s,a,b) -> fprintf ppf "If(\"%s\",%a,%a)" s print_t a print_t b
  | Eq(a,b) -> fprintf ppf "Eq(\"%s\",\"%s\")@?" a b
  | LE(a,b) -> fprintf ppf "LE(\"%s\",\"%s\")@?" a b
  | Get(a,b) -> fprintf ppf "Get(\"%s\",\"%s\")@?" a b
  | Put(a,b,c) -> fprintf ppf "Put(\"%s\",\"%s\",\"%s\")@?" a b c
  | ExtArray(a,t) -> fprintf ppf "ExtArray(\"%s\",%a)@?" a Type.print_t t
  
let print_fundef ppf = function
| {name=(s,t);args=sts;formal_fv=zts;body=b} ->
  fprintf ppf "{name=(\"%s\",%a);args=%a;formal_fv=%a;body=%a}@?"
    s Type.print_t t
    Syntax.print_sts sts
    Syntax.print_sts zts
    print_t b
let print_fundefs ppf ls = Type.prints print_fundef ppf ls
let print_prog ppf = function
| Prog(fundefs,t) -> fprintf ppf "Prog(%a,%a)" print_fundefs fundefs print_t t
(**
 * 自由変数集合の取得
 * 与えられた式内の自由変数の集合を取得する．
 * 例1)x + yならSet(x,y)が自由変数
 * 例2)let x = 1 in x + yなら Set(x,y) - x = Set(y)が自由変数である．
 *)
let rec freeVar (e:t): S.t =
  match e with
    | Unit | Int(_) -> S.empty
    | Add(x, y)
    | Sub(x, y)  -> S.of_list [x; y]
    | Let((x, t), e1, e2) -> S.union (freeVar e1) (S.remove x (freeVar e2))
    | Var(x) -> S.singleton x
    | MakeCls((x, t), { entry = l; actual_fv = ys }, e) -> S.remove x (S.union (S.of_list ys) (freeVar e))
    | AppCls(x, ys) -> S.of_list (x :: ys)
    | AppDir(_, xs) -> S.of_list xs
    | ExtFunApp(_, xs, _) -> S.of_list xs
    | Bool(_) -> S.empty
    | Eq(x, y) | LE(x, y) -> S.of_list [x; y]
    | If(x, a, b) -> S.add x (S.union (freeVar a) (freeVar b))
    | Get(x, y) -> S.of_list [x; y]
    | Put(x, y, z) -> S.of_list [x; y; z]
    | ExtArray(_,_) -> S.empty
let toplevel: fundef list ref = ref []
let stack:fundef list list ref = ref []
let push (c:fundef list) = 
  stack := c::!stack

let pop ():fundef list =
  match !stack with
  | v::vs -> 
    stack := vs;
    v
  | _ -> assert false

(**
 * クロージャ変換ルーチン本体
 * 基本的にはKからCへの変換をする．
 * 関数呼び出しは関数の集合(known)にあればCAppDirをなければ，CAppClsを呼び出す．
 * 関数の定義は複雑なのでソースを直接参照．
 *)
let rec visit(env:Type.t M.t) (known: S.t) (e:KNormal.t):t =
  match e with
  | KNormal.Unit -> Unit
  | KNormal.Int(i) -> Int(i)
  | KNormal.Add(x, y) -> Add(x, y)
  | KNormal.Sub(x, y) -> Sub(x, y)
  | KNormal.Var(x) -> Var(x)
  | KNormal.Let((x, t), e1, e2) ->
    Let((x, t), visit env known e1, visit (M.add x t env) known e2)
  | KNormal.LetRec({KNormal.name=(x, t);KNormal.args=yts;KNormal.body=e1}, e2) ->

    (* 新しい環境を作る *)
    let env_ = M.add x t env in

    (* e1をクロージャ変換する．自由変数も追加される． *)
    let (known_, e1_) =
      (** 関数に自由変数がないと仮定し、knownに追加してe1をクロージャ変換してみる **)
      (* トップレベルを保存 *)
      push(!toplevel);
      (* 分かっている集合を作る *)
      let known_ = S.add x known in
      (* とりあえずクロージャ変換してみる *)
      let e1_ = visit (M.add_list yts env_) known_ e1 in

      (* 自由変数がないか、変換結果e1_をチェック *)

      (* e1_内の自由変数から引数名を引いたものが自由変数 *)
      let zs = S.diff (freeVar e1_) (S.of_list (List.map fst yts)) in

      if S.is_empty zs then (
        let _ = pop() in
        (* 自由変数がないので只の関数 *)
        (known_, e1_)
      ) else (
        (* 自由変数がある場合はclosureが必要 *)
        (* toplevelの値を戻す *)
        toplevel := pop();
        (* 変換をやり直す *)
        let e1a = visit (M.add_list yts env_) known e1 in
        (known, e1a)
      )
    in

    (* e1_内の自由変数から関数名と引数名を引いた自由変数を求める *)
    let zs = S.elements 
      (S.diff 
        (freeVar e1_)
        (S.add x (S.of_list(List.map fst yts)))) in

    (* 関数をトップレベルに追加する *)
    let _ =
      (* zsに型を追加したztsを求める *)
      let zts = List.map (fun z -> (z, M.find z env_) ) zs in
      (* トップレベル関数を追加 *)
      toplevel := {name=(x, t); args=yts; formal_fv=zts; body=e1_ } :: !toplevel
    in

    (* 継続の式を変換 *)
    let e2_ = visit env_ known_ e2 in

    (* 関数名がe2_で使われているか？ *)
    if S.mem x (freeVar e2_)
    then (
      (* 使われているのでクロージャを生成する *)
      MakeCls((x, t), {entry=x; actual_fv=zs}, e2_)
    ) else (
      (* 使われていないのでクロージャは生成せず，e2_をそのまま返す *)
      e2_
    )
  | KNormal.App(x, ys) ->
    if S.mem x known then AppDir(x, ys) else AppCls(x, ys)
  | KNormal.ExtFunApp(x, ys, t) -> ExtFunApp(x, ys, t)
  | KNormal.Bool(b) -> Bool(b)
  | KNormal.If(x, e1, e2) -> If(x, visit env known e1, visit env known e2)
  | KNormal.Eq(e1, e2) -> Eq(e1, e2)
  | KNormal.LE(e1, e2) -> LE(e1, e2)
  | KNormal.Get(x, y) -> Get(x, y)
  | KNormal.Put(x, y, z) -> Put(x, y, z)
  | KNormal.ExtArray(x, t) -> ExtArray(x, t)

(**
 * クロージャ変換
 *)
let apply(e:KNormal.t): prog =
  toplevel := [];
  let e = visit M.empty S.empty e in
  Prog(List.rev !toplevel, e)
