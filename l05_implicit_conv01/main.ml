open Format
open Utils

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
        r := Some(Type.Int 64);
        Type.Int 64
      | Type.Var({contents=Some(t)} as r) ->
        let t1 = deref_type t in
        r := Some(t1);
        t1
      | t -> t

  (**
   * 型変数を消す
   *)
  let rec deref_term(e:t):t =

    let deref_id_type(a:(string * Type.t)):(string * Type.t) =
      match a with
      | (x, t) -> (x, deref_type t)
    in
    match e with
    | Add(t, e1, e2) -> Add(deref_type t, deref_term e1, deref_term e2)
    | Sub(t, e1, e2) -> Sub(deref_type t, deref_term e1, deref_term e2)
    | Cast(t, e1) -> Cast(deref_type t, deref_term e1)
    | Let(xt, e1, e2) -> Let(deref_id_type xt, deref_term e1, deref_term e2)
    | Print(e) -> Print(deref_term e)
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
    | (Type.Unit, Type.Unit) -> ()
    | (Type.Int n, Type.Int m) when n = m -> ()
    | (Type.Var({contents=r1}), Type.Var({contents=r2})) when(r1 == r2) -> ()
    | (Type.Var({contents=Some(t1)}), _) -> unify t1 t2
    | (_, Type.Var({contents=Some(t2)})) -> unify t1 t2
    | (Type.Var({contents=None} as r1), _) -> (* 一方が未定義の型変数の場合 *)
      if occur r1 t2 then raise(Unify(t1, t2))
      else r1 := Some(t2)
    | (_, Type.Var({contents=None} as r2)) ->
      if occur r2 t1 then raise(Unify(t1, t2))
      else r2 := Some(t1)
    | (_, _) -> raise(Unify(t1, t2))

  let cast e t t1 =
    match deref_type t, deref_type t1 with
    | (Type.Int n, Type.Int m) when n <> m ->
      Cast(t, e)
    | _ ->
      unify t t1;
      e

  let cast2 e1 e2 t1 t2 =
    match deref_type t1, deref_type t2 with
    | (Type.Int n, Type.Int m) when n < m ->
      Cast(t2, e1), e2
    | (Type.Int n, Type.Int m) when n > m ->
      e1, Cast(t1, e2)
    | _ ->
      unify t1 t2;
      e1, e2

  (**
   * 型推論
   * 式をトラバースしながら、単一化を進め型を返す．
   * 例) a + bの型は aもbもintでa+bもintなので aの型とintで単一化 bの型とintで単一化する．
   *)
  let rec infer (env:Type.t M.t) (e:t):(t * Type.t) =
    try
      match e with
      | Unit -> e, Type.Unit
      | Int(t, _) -> e, t
      | Add(t, e1, e2) ->
        let (e1,t1) = infer env e1 in
        let (e2,t2) = infer env e2 in
        unify t t1;
        let (e1, e2) = cast2 e1 e2 t1 t2 in
        Add(t, e1, e2), t
      | Sub(t, e1, e2) ->
        let (e1,t1) = infer env e1 in
        let (e2,t2) = infer env e2 in
        unify t t1;
        let (e1, e2) = cast2 e1 e2 t1 t2 in
        Sub(t, e1, e2), t
      | Let((x, t), e1, e2) ->
        let (e1, t1) = infer env e1 in
        let (e2, t2) = infer (M.add x t env) e2 in
        let e1 = cast e1 t t1 in
        Let((x, t), e1, e2), t2
      | Var(x) when M.mem x env -> (e, M.find x env)
      | Var(x) -> (* 外部変数 *)
        let t = Type.Var(ref None) in
        (* println("free variable "+ x + " assumed as external "+a+"."+t)*)
        extenv := M.add x t !extenv;
        e, t
      | Print(e) ->
        let (e,t) = infer env e in
        let e = cast e (Type.Int 64) t in
        Print(e), Type.Unit
      | Cast(t, e1) ->
        let (e1, t1) = infer env e1 in
        Cast(t, e1), t
    with
      | Unify(t1, t2) ->
        raise (Error(deref_term e, deref_type t1, deref_type t2))

  (* エントリポイント *)
  let apply (e:t): t =
    extenv := M.empty;

    (* 型推論 *)
    let e = try
      let (e, t) = infer M.empty e in
      unify (Type.Unit) t;
      e
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

module KNormal = struct

  type t =
    | Int of Type.t * int
    | Add of string * string
    | Sub of string * string
    | Print of string
    | Let of (string * Type.t) * t * t
    | Unit
    | Var of string
    | Cast of Type.t * string
  let insert_let (e, t) k = (* letを挿入する補助関数 (caml2html: knormal_insert) *)
    match e with
    | Var(x) -> k x
    | _ ->
      let x = genid("..") in
      let e', t' = k x in
      Let((x, t), e, e'), t'

  let rec visit(env:Type.t M.t)(e:Syntax.t):(t * Type.t) =
    match e with
      | Syntax.Unit -> (Unit, Type.Unit)
      | Syntax.Int(t, i) -> (Int(t, i), t)
      | Syntax.Add(t, e1, e2) ->
        insert_let (visit env e1) (fun x ->
          insert_let (visit env e2) (fun y ->
            (Add(x, y), t)
          )
        )
      | Syntax.Sub(t, e1, e2) ->
        insert_let (visit env e1) (fun x ->
          insert_let (visit env e2) (fun y ->
            (Sub(x, y), t)
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
      | Syntax.Var(x) when M.mem x env -> Var(x), M.find x env
      | Syntax.Var(x) -> (* 外部配列の参照 (caml2html: knormal_extarray) *)
        failwith (Printf.sprintf "external variable %s does not have an array type" x)
      | Syntax.Cast(t, e1) ->
        let e1, t1 = visit env e1 in
        let x = genid("..") in
        (Let((x,t1), e1, Cast(t, x)), t)
  let apply (e: Syntax.t): t =
    fst (visit M.empty e)

end

module Virtual = struct

  type r =
    | RL of Type.t * string
    | RN of Type.t * string

  type t =
    | Print of r
    | Bin of r * string * r * r
    | Cast of r * r
  let regid = function
    | RL (_,id) -> id
    | RN (_,id) -> id

  let regt = function
    | RL (t,_) -> t
    | RN (t,_) -> t

  let vs :t list ref = ref []

  let add (v:t): unit =
    vs := v :: !vs

  let bin env op x y =
    let rx = M.find x env in
    let r = RL(regt rx, genid("..")) in
    add(Bin(r, op, rx, M.find y env));
    r
  
  let rec visit (env)(k: KNormal.t): r =
    match k with
      | KNormal.Int(t, i) ->
        RN(t, string_of_int i)
      | KNormal.Add(x, y) -> bin env "add" x y
      | KNormal.Sub(x, y) -> bin env "sub" x y
      | KNormal.Let((aId,aT), bK, cK) ->
        let bR = visit env bK in
        visit (M.add aId bR env) (cK)
      | KNormal.Print(aId) ->
        add(Print(M.find aId env));
        RN(Type.Unit,"0")
      | KNormal.Unit -> RN(Type.Unit, "0")
      | KNormal.Var a ->
        M.find a env
      | KNormal.Cast(t,x) ->
        let rx = M.find x env in
        let r = RL(t, genid("..")) in
        add(Cast(r,rx));
        r
  let apply (k: KNormal.t): t list =
    vs := [];
    let _ = visit M.empty k in
    List.rev !vs

end

module Emit = struct

  open Virtual

  let p(r:r): string =
    match r with
      | RL(_,id) -> "%" ^ id
      | RN(_,id) -> id

  let pt(t:Type.t): string =
    match t with
    | Type.Int n -> "i" ^ string_of_int n
    | Type.Unit -> "void"
    | Type.Var _ -> assert false

  let ptr(r:r): string =
    match r with
      | RL(t,_) -> pt t
      | RN(t,_) -> pt t

  let pr(r:r): string =
    ptr r ^ " " ^ p r

  let emit(v: t) =
    match v with
      | Bin(id, op, a, b) ->
        asm_p(p(id) ^ " = " ^ op ^ " " ^ pr a ^ ", " ^ p(b))
      | Print(a) ->
        asm_p("call i64 @print(" ^ pr a ^ ") nounwind ssp")
      | Cast(r1,r2) ->
        (match (regt r1, regt r2) with
        | Type.Int(n), Type.Int(m) when n > m ->
          asm_p(p r1 ^" = sext "^pr r2^" to "^ ptr r1)
        | Type.Int(n), Type.Int(m) when n < m ->
          asm_p(p r1 ^" = trunc "^pr r2^" to "^ ptr r1)
        | _ ->
          asm_p(p r1 ^" = bitcast "^pr r2^" to "^ ptr r1)
        )
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

let print_exec cmd =
  match exec cmd with
  | (a,b,c) -> fprintf std_formatter "(%s,%s,%s)@." a b c

let parse src =
  let lexbuf = Lexing.from_string src in
  Parser.exp Lexer.token lexbuf

let compile output src =
  let ast = parse src in
  let ast = Typing.apply(ast) in
  let k = KNormal.apply(ast) in
  let vs = Virtual.apply(k) in
  Emit.apply output vs

let _ =
  let src = "print 1;print (2 + 3);print ((2+3)-2); let a:byte = 1+2 in print a; let b=a+1 in print b" in
  compile "a.ll" src;
  print_exec("llc a.ll -o a.s");
  print_exec("llvm-gcc -m64 a.s");
  print_exec("./a.out")
