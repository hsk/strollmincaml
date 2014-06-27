type t =
  | Int of int
  | Add of t * t
  | Sub of t * t
  | Neg of t
  | Float of float
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | FNeg of t
  | Bool of bool
  | Not of t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (string * Type.t) * t * t
  | Unit
  | Var of string
  | LetRec of fundef * t
  | App of t * t list
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
  | Tuple of t list
  | LetTuple of (string * Type.t) list * t * t
and fundef = { name : string * Type.t; args : (string * Type.t) list; body : t }

open Format

let rec print_t ppf = function
  | Int(i) -> fprintf ppf "Int(%d)@?" i
  | Add(a,b) -> fprintf ppf "Add(%a,%a)@?" print_t a print_t b
  | Sub(a,b) -> fprintf ppf "Sub(%a,%a)@?" print_t a print_t b
  | Neg(a) -> fprintf ppf "Neg(%a)@?" print_t a
  | Float(f) -> fprintf ppf "Int(%f)@?" f
  | FAdd(a,b) -> fprintf ppf "FAdd(%a,%a)@?" print_t a print_t b
  | FSub(a,b) -> fprintf ppf "FSub(%a,%a)@?" print_t a print_t b
  | FMul(a,b) -> fprintf ppf "FMul(%a,%a)@?" print_t a print_t b
  | FDiv(a,b) -> fprintf ppf "FDiv(%a,%a)@?" print_t a print_t b
  | FNeg(a) -> fprintf ppf "FNeg(%a)@?" print_t a
  | Bool(b) -> fprintf ppf "Bool(%b)@?" b
  | Not(t) -> fprintf ppf "Not(%a)" print_t t
  | Eq(a,b) -> fprintf ppf "Eq(%a,%a)@?" print_t a print_t b
  | LE(a,b) -> fprintf ppf "LE(%a,%a)@?" print_t a print_t b
  | If(a,b,c) -> fprintf ppf "If(%a,%a,%a)@?" print_t a print_t b print_t c
  | Let((s,t),a,b) -> fprintf ppf "Let((\"%s\",%a),%a,%a)" s Type.print_t t print_t a print_t b
  | Unit -> fprintf ppf "Unit@?"
  | Var(a) -> fprintf ppf "Var(\"%s\")@?" a
  | LetRec({name=(s,t);args=args;body=e1},e2) ->
    fprintf ppf "LetRec({name=(\"%s\",%a);args=%a;body=%a},%a)"
      s Type.print_t t print_sts args print_t e1 print_t e2
  | App(t,ts) -> fprintf ppf "App(%a,%a)" print_t t print_ts ts
  | Array(a,b) -> fprintf ppf "Array(%a,%a)@?" print_t a print_t b
  | Get(a,b) -> fprintf ppf "Get(%a,%a)@?" print_t a print_t b
  | Put(a,b,c) -> fprintf ppf "Put(%a,%a,%a)@?" print_t a print_t b  print_t c
  | Tuple(ts) -> fprintf ppf "Tuple(%a)" print_ts ts
  | LetTuple(sts,e1,e2) ->
    fprintf ppf "LetTuple(%a,%a,%a)"
      print_sts sts print_t e1 print_t e2
and print_st ppf = function
  | (s,t) -> fprintf ppf "(\"%s\",%a)" s Type.print_t t
and print_sts ppf ls = Type.prints print_st ppf ls
and print_ts ppf ls = Type.prints print_t ppf ls

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
  | Ast.Var(id) -> Var(id)
  (*  | Ast.Ref(Ast.Var(id)) when S.mem id env -> Var(id) *)
  | Ast.Array (e1, e2) -> Array(visit env e1, visit env e2)
  | Ast.Ref(Ast.Var(id)) when S.mem id env -> Var(id)
  | Ast.Ref(e1) -> Array(Int 1, visit env e1)

  | Ast.Put(e1, e2, e3) ->
    (match visit env e1 with
    | Get(e1,Int 0) ->
      Put(e1, visit env e2, visit env e3)
    | e1 ->
      Put(e1, visit env e2, visit env e3)
    )
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
  | Ast.Get (e1, e2) -> Get(visit env e1, visit env e2)
  | Ast.Tuple es -> Tuple(List.map (visit env) es)
  | Ast.LetTuple (ns, e1, e2) -> LetTuple(ns, visit env e1, visit env e2)

  | Ast.App(e1, es) -> App(visit env e1, List.map (visit env) es)

let apply (e: Ast.t):t =
  visit S.empty e

