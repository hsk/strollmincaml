(**
 * 型推論
 *)
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
    | Type.Fun(t1s, t2) ->
      Type.Fun(List.map deref_type t1s, deref_type t2)
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
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec({ name = deref_id_type xt;
         args = List.map deref_id_type yts;
         body = deref_term e1 },
       deref_term e2)
  | App(e, es) -> App(deref_term e, List.map deref_term es)
  | Unit | Var _ | Int _ -> e

(* 単一化 *)
let rec unify (t1:Type.t) (t2:Type.t) =

  (* 出現チェック *)
  let rec occur(r1:Type.t option ref)(r2:Type.t):bool =
    match r2 with
    | Type.Fun(t2s, t2) -> List.exists (occur r1) t2s || occur r1 t2
    | Type.Var(r2) when (r1 == r2) -> true
    | Type.Var({contents=None}) -> false
    | Type.Var({contents=Some(t2)}) -> occur r1 t2
    | _ -> false
  in

  match (t1, t2) with 
  | (Type.Unit, Type.Unit) | (Type.Int, Type.Int) -> ()
  | Type.Fun(t1s, t1'), Type.Fun(t2s, t2') ->
      (try
        List.iter2 unify t1s t2s
      with
        | Invalid_argument("List.iter2") -> raise (Unify(t1, t2))
      );
      unify t1' t2'
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
      | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
        let env = M.add x t env in
        unify t (Type.Fun(List.map snd yts, infer (M.add_list yts env) e1));
        infer env e2
      | App(e, es) ->
        let t = Type.Var(ref None) in
        unify (infer env e) (Type.Fun(List.map (infer env) es, t));
        t
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
