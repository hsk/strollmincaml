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
        r := Some(Type.Int);
        Type.Int
      | Type.Var({contents=Some(t)} as r) ->
        let t1 = deref_type(t) in
        r := Some(t1);
        t1
      | Type.Fun(t1s, t2) ->
        Type.Fun(List.map deref_type t1s, deref_type t2)
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
      | Type.Var(r2) when (r1 == r2) -> true
      | Type.Var({contents=None}) -> false
      | Type.Var({contents=Some(t2)}) -> occur r1 t2
      | Type.Fun(t2s, t2) -> List.exists (occur r1) t2s || occur r1 t2
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

end

module KNormal = struct

  type t =
    | Int of int
    | Add of string * string
    | Sub of string * string
    | Let of (string * Type.t) * t * t
    | Unit
    | Var of string
    | LetRec of fundef * t
    | App of string * string list
    | ExtFunApp of string * string list * Type.t
  and fundef = {
    name : string * Type.t;
    args : (string * Type.t) list;
    body : t }

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
      | Syntax.Let((x,t), e1, e2) ->
        let e1', t1 = visit env e1 in
        let e2', t2 = visit (M.add x t env) e2 in
        Let((x, t), e1', e2'), t2
      | Syntax.Var(x) when M.mem x env -> Var(x), M.find x env
      | Syntax.Var(x) -> (* 外部配列の参照 (caml2html: knormal_extarray) *)
        failwith (Printf.sprintf "external variable %s does not have an array type" x)
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

  let apply (e: Syntax.t): t =
    fst (visit M.empty e)

end

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

module Virtual = struct

  type r =
    | RL of Type.t * string
    | RN of Type.t * string
    | RG of Type.t * string

  type t =
    | Call of r * r * r list
    | Bin of r * string * r * r
    | Ret of r

  type fundef =
     {name : string; args : (string * Type.t) list; body : t list; ret : Type.t}

  type prog = Prog of fundef list

  let regid = function
    | RL (_,id) -> id
    | RN (_,id) -> id
    | RG (_,id) -> id

  let regt = function
    | RL (t,_) -> t
    | RN (t,_) -> t
    | RG (t,_) -> t

  let vs :t list ref = ref []

  let add (v:t): unit =
    vs := v :: !vs

  let bin env op x y =
    let rx = M.find x env in
    let r = RL(regt rx, genid("..")) in
    add(Bin(r, op, rx, M.find y env));
    r
  
  let rec visit (env)(c: Closure.t): r =
    match c with
      | Closure.Int(i) ->
        RN(Type.Int, string_of_int i)
      | Closure.Add(x, y) -> bin env "add" x y
      | Closure.Sub(x, y) -> bin env "sub" x y
      | Closure.Let((aId,aT), bK, cK) ->
        let bR = visit env bK in
        visit (M.add aId bR env) (cK)
      | Closure.Unit -> RN(Type.Unit, "0")
      | Closure.Var a ->
        M.find a env
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

end

module Emit = struct

  open Virtual

  let p(r:r): string =
    match r with
      | RL(_,id) -> "%" ^ id
      | RN(_,id) -> id
      | RG(_,id) -> "@" ^ id

  let rec pt(t:Type.t): string =
    match t with
    | Type.Int -> "i64"
    | Type.Unit -> "i64"
    | Type.Fun(ts,t) -> pt t ^ "(" ^ String.concat ", " (List.map pt ts) ^ ")*"
    | Type.Tuple(ts) -> "{" ^ String.concat ", " (List.map pt ts) ^ "}"
    | Type.Var _ -> assert false

  let ptr(r:r): string =
    match r with
      | RL(t,_) -> pt t
      | RN(t,_) -> pt t
      | RG(t,_) -> pt t

  let pr(r:r): string =
    ptr r ^ " " ^ p r

  let emit(v: t) =
    match v with
      | Bin(id, op, a, b) ->
        asm_p(p(id) ^ " = " ^ op ^ " " ^ pr a ^ ", " ^ p(b))
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

  let apply(file: string) (Prog(fundefs)):unit =
    asm_open(file);

    List.iter (fun
      {name = x; args = args; body = vs; ret = t} ->
      let args = String.concat ", " (List.map (fun (s,t)-> pt t ^ " %" ^ s) args) in
      asm("define "^pt t^" @"^x^"("^args^") nounwind ssp {");
      asm("entry:");
      List.iter (fun v -> emit(v)) vs;
      asm("}");

    ) fundefs;

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

let _ =
  let src = "let rec f x = x+1 in print (f 1); print (2 + 3);print ((2+3)-2); let a = 1+2 in print a" in
  compile "a.ll" src;
  print_exec("llc a.ll -o a.s");
  print_exec("llvm-gcc -m64 a.s");
  print_exec("./a.out")
