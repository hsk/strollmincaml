open Format
open Utils
open Syntax

type t =
  | Int of int
  | Add of string * string
  | Sub of string * string
  | Neg of string
  | Float of float
  | FAdd of string * string
  | FSub of string * string
  | FMul of string * string
  | FDiv of string * string
  | FNeg of string
  | Bool of bool
  | If of string * t * t
  | LE of string * string
  | Eq of string * string
  | Let of (string * Type.t) * t * t
  | Unit
  | Var of string
  | LetRec of fundef * t
  | App of string * string list
  | ExtFunApp of string * string list * Type.t
  | Get of string * string
  | Put of string * string * string
  | ExtArray of string * Type.t
  | Cast of string * Type.t
  | LetTuple of (string * Type.t) list * string * t
  | Tuple of string list
and fundef = {
  name : string * Type.t;
  args : (string * Type.t) list;
  body : t }

let rec print_t ppf = function
  | Int i -> fprintf ppf "Int(%d)@?" i
  | Add(a,b) -> fprintf ppf "Add(\"%s\",\"%s\")@?" a b
  | Sub(a,b) -> fprintf ppf "Sub(\"%s\",\"%s\")@?" a b
  | Neg(a) -> fprintf ppf "Neg(\"%s\")" a
  | Float(f) -> fprintf ppf "Float(%f)@?" f
  | FAdd(a,b) -> fprintf ppf "FAdd(\"%s\",\"%s\")@?" a b
  | FSub(a,b) -> fprintf ppf "FSub(\"%s\",\"%s\")@?" a b
  | FMul(a,b) -> fprintf ppf "FMul(\"%s\",\"%s\")@?" a b
  | FDiv(a,b) -> fprintf ppf "FDiv(\"%s\",\"%s\")@?" a b
  | FNeg(a) -> fprintf ppf "FNeg(\"%s\")@?" a
  | Bool(b) ->  fprintf ppf "Bool(%b)@?" b
  | If(s,a,b) -> fprintf ppf "If(\"%s\",%a,%a)" s print_t a print_t b
  | Eq(a,b) -> fprintf ppf "Eq(\"%s\",\"%s\")@?" a b
  | LE(a,b) -> fprintf ppf "LE(\"%s\",\"%s\")@?" a b
  | Let((s,t),a,b) -> fprintf ppf "Let((\"%s\",%a),%a,%a)@?" s Type.print_t t print_t a print_t b
  | Unit -> fprintf ppf "Unit@?"
  | Var(a) -> fprintf ppf "Var(\"%s\")@?" a
  | LetRec({name=(s,t);args=sts;body=a},b) ->
    fprintf ppf "LetRec({name=(\"%s\",%a);args=%a;body=%a},%a)@?"
      s Type.print_t t
      Syntax.print_sts sts
      print_t a print_t b
  | App(s,ss) -> fprintf ppf "App(\"%s\",[%s])@?" s (String.concat "; " ss)
  | ExtFunApp(s,ss,t) -> fprintf ppf "ExtFunApp(\"%s\",[%s],%a)@?" s (String.concat "; " ss) Type.print_t t
  | Get(a,b) -> fprintf ppf "Get(\"%s\",\"%s\")@?" a b
  | Put(a,b,c) -> fprintf ppf "Put(\"%s\",\"%s\",\"%s\")@?" a b c
  | ExtArray(s,t) -> fprintf ppf "ExtArray(\"%s\",%a)@?" s Type.print_t t
  | Cast(s,t) -> fprintf ppf "Cast(\"%s\",%a)@?" s Type.print_t t
  | LetTuple(sts,s,t) ->
    fprintf ppf "LetTuple(%a,\"%s\",%a)@?"
      Syntax.print_sts sts
      s print_t t
  | Tuple(ss) -> fprintf ppf "Tuple([%s])@?" (String.concat "; " ss)

let insert_let (e, t) k = (* letを挿入する補助関数 (caml2html: knormal_insert) *)
  match e with
  | Var(x) -> k x
  | _ ->
    let x = genid("..") in
    let e', t' = k x in
    Let((x, t), e, e'), t'

let rec visit(env:Type.t M.t)(e:Syntax.t):(t * Type.t) =
  fprintf std_formatter "visit %a@." Syntax.print_t e;
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
    | Syntax.Neg(e1) ->
      insert_let (visit env e1) (fun x ->
        (Neg(x), Type.Int)
      )
    | Syntax.Float(f) -> (Float(f), Type.Float)
    | Syntax.FAdd(e1, e2) ->
      insert_let (visit env e1) (fun x ->
        insert_let (visit env e2) (fun y ->
          (FAdd(x, y), Type.Float)
        )
      )
    | Syntax.FSub(e1, e2) ->
      insert_let (visit env e1) (fun x ->
        insert_let (visit env e2) (fun y ->
          (FSub(x, y), Type.Float)
        )
      )
    | Syntax.FMul(e1, e2) ->
      insert_let (visit env e1) (fun x ->
        insert_let (visit env e2) (fun y ->
          (FMul(x, y), Type.Float)
        )
      )
    | Syntax.FDiv(e1, e2) ->
      insert_let (visit env e1) (fun x ->
        insert_let (visit env e2) (fun y ->
          (FDiv(x, y), Type.Float)
        )
      )
    | Syntax.FNeg(e1) ->
      insert_let (visit env e1) (fun x ->
          (FNeg(x), Type.Float)
      )
    | Syntax.Bool(b) -> Bool b, Type.Bool
    | Syntax.Not(e) ->
      visit env (Syntax.If(e, Syntax.Bool(false), Syntax.Bool(true)))
    | Syntax.Eq (e1, e2) ->
      insert_let (visit env e1) (fun x ->
        insert_let (visit env e2) (fun y ->
          (Eq(x,y), Type.Bool)
        )
      )
    | Syntax.LE (e1, e2) ->
      insert_let (visit env e1) (fun x ->
        insert_let (visit env e2) (fun y ->
          (LE(x,y), Type.Bool)
        )
      )
    | Syntax.If(e1, e2, e3) ->
      insert_let (visit env e1) (fun x ->
        let e2', t2 = visit env e2 in
        let e3', t3 = visit env e3 in
        If (x, e2', e3'), t2
      )
    | Syntax.Let((x,t), e1, e2) ->
      let e1', t1 = visit env e1 in
      let e2', t2 = visit (M.add x t env) e2 in
      Let((x, t), e1', e2'), t2
    | Syntax.Var(x,t) when M.mem x env -> Var(x), t
    | Syntax.Var(x,t) -> (* 外部配列の参照 (caml2html: knormal_extarray) *)
      (match M.find x !Typing.extenv with
      | Type.Array(_) as t -> ExtArray(x, t), t
      | _ -> failwith (Printf.sprintf "external variable %s does not have an array type" x))
    | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2) ->
      let env' = M.add x t env in
      let e2', t2 = visit env' e2 in
      let e1', t1 = visit (M.add_list yts env') e1 in
      LetRec({ name = (x, t); args = yts; body = e1' }, e2'), t2
    | Syntax.App(Syntax.Var(f,t), e2s) when not (M.mem f env) ->
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
    | Syntax.Array(e1, e2) ->
      insert_let (visit env e1) (fun x ->
        let _, t2 as g_e2 = visit env e2 in
        insert_let g_e2 (fun y ->
          let l =
            match t2 with
            | Type.Float -> "create_float_array"
            | _ -> "create_array"
          in
          match t2 with
          | Type.Float | Type.Int ->
            ExtFunApp(l, [x; y], Type.Array(t2)),Type.Array(t2)
          | _ ->
            insert_let(Cast(y, Type.Int),Type.Int) (fun z ->
              insert_let
                (ExtFunApp(l, [x; z], Type.Array(Type.Int)),Type.Array(Type.Int))
              (fun a ->
                Cast(a, Type.Array(t2)), Type.Array(t2)
              )
            )
        )
      )
    | Syntax.Get(e1, e2) ->
      (match visit env e1 with
      | _, Type.Array(t) as g_e1 ->
        insert_let g_e1 (fun x ->
          insert_let (visit env e2) (fun y ->
            Get(x, y), t
          )
        )
      | _ -> assert false
      )
    | Syntax.Put(e1, e2, e3) ->
      insert_let (visit env e1) (fun x ->
        insert_let (visit env e2) (fun y ->
          insert_let (visit env e3) (fun z ->
            Put(x, y, z), Type.Unit)
        )
      )
    | Syntax.Tuple(es) ->
      let rec bind (xs:string list) (ts:Type.t list) = function (* "xs" and "ts" are identifiers and types for the elements *)
        | [] -> Tuple(List.rev xs), Type.Tuple(List.rev ts)
        | e :: es ->
          let _, t as g_e = visit env e in
          insert_let g_e (fun x ->
            bind (x::xs) (t::ts) es
          )
      in
        bind [] [] es
    | Syntax.LetTuple(xts, e1, e2) ->
      insert_let (visit env e1) (fun y ->
        let e2', t2 = visit (M.add_list xts env) e2 in
        LetTuple(xts, y, e2'), t2
      )

let apply (e: Syntax.t): t =
  fst (visit M.empty e)
