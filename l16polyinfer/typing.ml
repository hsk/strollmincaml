(**
 * 型推論
 *)

open Syntax
open Format
open Utils

let extenv:Type.t M.t ref = ref M.empty


(**
 * 型変数を消す
 *)
let rec deref_type(x:Type.t):Type.t =
  match x with
    | Type.Var({contents=Type.Link(t)} as r) ->
      let t1 = deref_type(t) in
      r := Type.Link(t1);
      t1
    | Type.Fun(t1s, t2) ->
      Type.Fun(List.map deref_type t1s, deref_type t2)
    | Type.Array(t) -> Type.Array(deref_type t)
    | Type.Tuple(ts) -> Type.Tuple(List.map deref_type ts)
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
  | Neg(e) -> Neg(deref_term e)
  | Float _ -> e
  | FAdd(e1, e2) -> FAdd(deref_term e1, deref_term e2)
  | FSub(e1, e2) -> FSub(deref_term e1, deref_term e2)
  | FMul(e1, e2) -> FMul(deref_term e1, deref_term e2)
  | FDiv(e1, e2) -> FDiv(deref_term e1, deref_term e2)
  | FNeg(e) -> FNeg(deref_term e)
  | Let(xt, e1, e2) -> Let(deref_id_type(xt), deref_term(e1), deref_term(e2))
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec({ name = deref_id_type xt;
         args = List.map deref_id_type yts;
         body = deref_term e1 },
       deref_term e2)
  | App(e, es) -> App(deref_term e, List.map deref_term es)
  | Unit | Int _ | Bool _ -> e
  | Var(x,t) -> Var(x, deref_type t)
  | Not(e) -> Not(deref_term e)
  | Eq(e1, e2) -> Eq(deref_term e1, deref_term e2)
  | LE(e1, e2) -> LE(deref_term e1, deref_term e2)
  | If(e1, e2, e3) -> If(deref_term e1, deref_term e2, deref_term e3)
  | Array(e1, e2) -> Array(deref_term e1, deref_term e2)
  | Get(e1, e2) -> Get(deref_term e1, deref_term e2)
  | Put(e1, e2, e3) -> Put(deref_term e1, deref_term e2, deref_term e3)
  | Tuple(es) -> Tuple(List.map deref_term es)
  | LetTuple(xts, e1, e2) -> LetTuple(List.map deref_id_type xts, deref_term e1, deref_term e2)

let level = ref 1

let enter_level () =
  incr level
let leave_level () =
  decr level

let cut t =
  match t with
  | Type.Array t -> t
  | _ -> assert false

let newvar () =
  Type.Var(ref(Type.Unbound(genid(".."), !level)))

let rec occurs tvr t =
  match t with
  | Type.Var tvr' when tvr==tvr' ->
    fprintf str_formatter "occurs check error %a %a"
      Type.print_tv !tvr Type.print_tv !tvr';
    failwith(flush_str_formatter())
  | Type.Var({contents=Type.Unbound(name,l)} as tv) ->
    let min_level =
      match !tvr with
      | Type.Unbound(_,l1) -> min l l1
      | _ -> l
    in tv := Type.Unbound(name, min_level)
  | Type.Var{contents=Type.Link t} -> occurs tvr t
  | Type.Fun(ts,t2) -> List.iter (occurs tvr) ts; occurs tvr t2
  | _ -> ()

let rec unify t1 t2 =
  match (t1,t2) with
  | (t1,t2) when t1 == t2 -> ()
  | (Type.Var{contents=Type.Link t1}, t2)
  | (t1, Type.Var{contents=Type.Link t2}) ->
    unify t1 t2
  | (Type.Var({contents=Type.Unbound _}as tv), t)
  | (t, Type.Var({contents=Type.Unbound _}as tv)) ->
    (try
      occurs tv t;
    with
      _ ->
      fprintf str_formatter "occurs check error %a %a" 
        Type.print_tv !tv Type.print_t t;
      failwith(flush_str_formatter())
    );
    tv := Type.Link t
  | (Type.Fun(l1,l2),Type.Fun(r1,r2)) ->
    List.iter (fun (l,r) -> unify l r) (List.combine l1 r1);
    unify l2 r2
  | (Type.Array t1,Type.Array t2) -> unify t1 t2
  | (Type.Int, Type.Int) -> () 
  | (Type.Tuple ts1, Type.Tuple ts2) -> List.iter2 unify ts1 ts2
  | _ ->
    fprintf str_formatter "invalid unify %a %a@."
      Type.print_t t1 Type.print_t t2;
    failwith(flush_str_formatter())

let rec gen(ty:Type.t):Type.t =
  match ty with
  | Type.Var{contents=Type.Unbound (name,l)} when l > !level ->
    Type.QVar name
  | Type.Var{contents=Type.Link t} -> gen t
  | Type.Fun(ts,t2) -> Type.Fun(List.map gen ts, gen t2)
  | Type.Array(t) -> Type.Array(gen(t))
  | ty -> ty

let rec instance env t =
  match t with
  | Type.QVar(name) ->
    (try
      (env, M.find name env)
    with
    | _ ->
      let t = newvar() in
      (M.add name t env, t)
    )
  | Type.Fun(ts,t2) ->
    let (env,ts) = List.fold_left
      (fun (env,ls) t ->
        let (env, t) = instance env t in
        (env, t::ls)
      )
      (env, [])
      ts
    in
    let (env,t2) = instance env t2 in
    (env, Type.Fun(List.rev ts, t2))
  | Type.Array(t) ->
    let (env,t) = instance env t in
    (env, Type.Array(t))
  | t -> (env, t)

let inst t =
  let (_,t) = instance M.empty t in
  t

let rec visit (env:Type.t M.t) (e:t):Type.t =
  (*fprintf std_formatter "visit %a@." print_e e;*)
  match e with
  | Var(x,vt) when M.mem x env ->
    let t = M.find x env in
    let t = inst(t) in
    unify vt t;
    t
  | Var(x,t) -> (* 外部変数 *)
    (* println("free variable "+ x + " assumed as external "+a+"."+t)*)
    extenv := M.add x t !extenv;
    t

  | Let((id,t),e1,e2) ->
    enter_level();
    let te1 = visit env e1 in
    (try
      unify t te1
    with ee ->
      fprintf std_formatter "let %s type error@." id;
    raise ee);

    leave_level();
    visit (M.add id (gen te1) env) e2
  | App(e1,es) ->
    let te1 = visit env e1 in
    let te2 = List.map (visit env) es in
    let t = newvar() in
    (try
      unify te1 (Type.Fun(te2,t));
    with ee ->
      fprintf std_formatter "app type error@.";
      raise ee
    );
    t
  | LetRec({name=(id,t);args=xs;body=e1},e2) ->

      enter_level();
      let (env,ts) = List.fold_left
        (fun (env,ts) (x,t1) ->
          let t = newvar() in
          unify t1 t;
          (M.add x t env, t::ts)
        )
        (env,[])
        xs
      in
      let ter = visit (M.add id t env) e1 in
      let te1 = Type.Fun(List.rev ts ,ter) in
      (try
        unify t te1;
      with ee ->
        fprintf std_formatter "let rec %s@.t =%a@.t1=%a@.type error@." id Type.print_t t Type.print_t te1;
        raise ee
      );

      leave_level();
      visit (M.add id (gen te1) env) e2

  | Int(i) ->
    Type.Int
  | Float _ ->
    Type.Float
  | Neg e ->
    unify Type.Int (visit env e);
    Type.Int
  | FNeg e ->
    unify Type.Float (visit env e);
    Type.Float
  | Bool _ -> Type.Bool
  | Not e ->
    unify Type.Bool (visit env e);
    Type.Bool

  | Add(e1,e2)
  | Sub(e1,e2) ->
    let ty1 = visit env e1 in
    let ty2 = visit env e2 in
    unify Type.Int ty1;
    unify ty1 ty2;
    ty1
  | FAdd(e1,e2)
  | FSub(e1,e2)
  | FMul(e1,e2)
  | FDiv(e1,e2) ->
    let ty1 = visit env e1 in
    let ty2 = visit env e2 in
    unify Type.Float ty1;
    unify ty1 ty2;
    ty1

  | Eq(e1,e2)
  | LE(e1,e2) ->
    let ty1 = visit env e1 in
    let ty2 = visit env e2 in
    unify ty1 ty2;
    Type.Bool
  
  | If (e1, e2, e3) ->
    unify Type.Bool (visit env e1);
    let t2 = visit env e2 in
    let t3 = visit env e3 in
    unify t2 t3;
    t2

  | Get (e1, e2) ->
    let t = Type.gentyp() in
    let t1 = visit env e1 in
    let t2 = visit env e2 in
    unify (Type.Array(t)) t1;
    unify Type.Int t2;
    t
  | Tuple es ->
    Type.Tuple(List.map (visit env) es)

  | LetTuple(xts, e1, e2) ->
    unify (Type.Tuple(List.map snd xts)) (visit env e1);
    visit (M.add_list xts env) e2

  | Unit -> Type.Unit

  | Array(e1,e2) ->
    unify Type.Int (visit env e1);
    Type.Array(visit env e2)

  | Put(e1,e2,e3) ->
    let t1 = visit env e1 in
    let t3 = visit env e3 in
    unify Type.Int (visit env e2);
    unify t1 (Type.Array(t3));
    t3

let rec genE(e:t):t =
  match e with
    | App(e1,es) -> App(genE(e1), List.map genE es)
    | LetRec({ name = (x, t); args = args; body = e },e2) ->
      LetRec({
        name = (x, gen(t)); 
        args = List.map(fun (s,t)->(s,gen t)) args;
        body = genE(e)
      }, genE(e2))
    | Let((s,t),e1,e2) -> Let((s,gen(t)),genE(e1),genE(e2))
    | Add(e1,e2) -> Add(genE(e1),genE(e2))
    | Sub(e1,e2) -> Sub(genE(e1),genE(e2))
    | FAdd(e1,e2) -> FAdd(genE(e1),genE(e2))
    | FSub(e1,e2) -> FSub(genE(e1),genE(e2))
    | FMul(e1,e2) -> FMul(genE(e1),genE(e2))
    | FDiv(e1,e2) -> FDiv(genE(e1),genE(e2))
    | Eq(e1,e2) -> Eq(genE(e1),genE(e2))
    | LE(e1,e2) -> LE(genE(e1),genE(e2))
    | If (e1, e2, e3) -> If(genE(e1),genE(e2),genE(e3))
    | Put (e1, e2, e3)-> Put(genE(e1),genE(e2),genE(e3))
    | Tuple es -> Tuple(List.map genE es)
    | LetTuple (x, e1, e2) -> LetTuple(x,genE(e1),genE(e2))
    | Array(e1,e2) -> Array(genE(e1),genE(e2))
    | Not e -> Not(genE(e))
    | Neg e -> Neg(genE(e))
    | FNeg e -> FNeg(genE(e))
    | Get(e1,e2) -> Get(genE(e1),genE(e2))      
    | Var(a,t) -> Var(a, gen t)
    | Int _
    | Float _
    | Bool _
    | Unit -> e

let rec instE(e:t):t =
  match e with
    | App(e1,es) -> App(instE(e1), List.map instE es)
    | LetRec({ name = (x, t); args = args; body = e },e2) ->
      LetRec({
        name = (x, inst(t)); 
        args = List.map(fun (s,t)->(s,inst t)) args;
        body = instE(e)
      }, instE(e2))
    | Let((s,t),e1,e2) -> Let((s,inst(t)),instE(e1),instE(e2))
    | Add(e1,e2) -> Add(instE(e1),instE(e2))
    | Sub(e1,e2) -> Sub(instE(e1),instE(e2))
    | FAdd(e1,e2) -> FAdd(instE(e1),instE(e2))
    | FSub(e1,e2) -> FSub(instE(e1),instE(e2))
    | FMul(e1,e2) -> FMul(instE(e1),instE(e2))
    | FDiv(e1,e2) -> FDiv(instE(e1),instE(e2))
    | Eq(e1,e2) -> Eq(instE(e1),instE(e2))
    | LE(e1,e2) -> LE(instE(e1),instE(e2))
    | If (e1, e2, e3) -> If(instE(e1),instE(e2),instE(e3))
    | Put (e1, e2, e3)-> Put(instE(e1),instE(e2),instE(e3))
    | Tuple es -> Tuple(List.map instE es)
    | LetTuple (x, e1, e2) -> LetTuple(x,instE(e1),instE(e2))
    | Array(e1,e2) -> Array(instE(e1),instE(e2))
    | Not e -> Not(instE(e))
    | Neg e -> Neg(instE(e))
    | FNeg e -> FNeg(instE(e))
    | Get(e1,e2) -> Get(instE(e1),instE(e2))      
    | Var(a,t) -> Var(a, inst t)
    | Int _
    | Float _
    | Bool _
    | Unit -> e

let apply(e:t):t =
  extenv := M.add "print" (Type.Fun([Type.Int],Type.Int)) M.empty;
  extenv := M.add "create_array" (Type.Fun([Type.Int;Type.Int],Type.Array(Type.Int))) !extenv;
  (*let _ = visit(M.add "print" (Type.Fun([Type.Int],Type.Int)) M.empty) e in*)
  let _ = visit !extenv e in
  deref_term e
