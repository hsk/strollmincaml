type atom =
  | Var of string
  | Root of string

type closure = { entry : string; actual_fv : atom list }

type t =
  | Unit
  | Int of int
  | Add of atom * atom
  | Sub of atom * atom
  | Float of float
  | Neg of atom
  | FAdd of atom * atom
  | FSub of atom * atom
  | FMul of atom * atom
  | FDiv of atom * atom
  | FNeg of atom
  | Bool of bool
  | If of atom * t * t
  | LE of atom * atom
  | Eq of atom * atom
  | Let of (atom * Type.t) * t * t
  | Atom of atom
  | AppDir of string * atom list
  | ExtFunApp of string * atom list * Type.t
  | MakeCls of (atom * Type.t) * closure * t
  | AppCls of atom * atom list
  | ExtArray of string * Type.t
  | LetTuple of (atom * Type.t) list * atom * t
  | Tuple of atom list
  | Get of atom * atom
  | Put of atom * atom * atom

let rec triggers = function
  | Closure.ExtFunApp(_,_,Type.Array(t)) -> true
  | Closure.Tuple(_) -> true
  | Closure.If (_, e1, e2)
  | Closure.Let (_, e1, e2) -> triggers e1 || triggers e2
  | Closure.MakeCls _ -> true
  | Closure.LetTuple (_, _, e) -> triggers e
  | _ -> false

let remove_list l s =
  List.fold_right S.remove l s

let rec roots e =
  match e with
  | Closure.Tuple(xs) -> S.of_list xs
  | Closure.ExtFunApp(_,xs,Type.Array(t)) -> S.of_list xs
  | Closure.If (_, e1, e2) -> S.union (roots e1) (roots e2)
  | Closure.Let ((id, _), e1, e2) when triggers e1 ->
    S.remove id (S.union (roots e1) (Closure.freeVar e2))
  | Closure.Let ((id, _), _, e2) -> S.remove id (roots e2)
  | Closure.MakeCls ((_,_),(clos),_) -> S.of_list clos.Closure.actual_fv
  | Closure.LetTuple (idtl, _, e) -> remove_list (List.map (fun (id, _) -> id) idtl) (roots e)
  | _ -> S.empty

let is_gc_type = function
  | Type.Fun _ | Type.Tuple _ | Type.Array _ -> true
  | _ -> false

let add x t env r =
  if S.mem x r && is_gc_type t then
    (Printf.eprintf "%s " x;
    (Root x, M.add x true env))
  else
    (Var x, M.add x false env)

let g_atom env id =
  let is_root = M.find id env in
  if is_root then Root id
  else Var id

let rec visit env = function
  | Closure.Add(a, b) -> Add(g_atom env a, g_atom env b)
  | Closure.Sub(a, b) -> Sub(g_atom env a, g_atom env b)
  | Closure.Neg(a) -> Neg(g_atom env a)
  | Closure.FAdd(a, b) -> FAdd(g_atom env a, g_atom env b)
  | Closure.FSub(a, b) -> FSub(g_atom env a, g_atom env b)
  | Closure.FMul(a, b) -> FMul(g_atom env a, g_atom env b)
  | Closure.FDiv(a, b) -> FDiv(g_atom env a, g_atom env b)
  | Closure.FNeg(a) -> FNeg(g_atom env a)
  | Closure.LE(a, b) -> LE(g_atom env a, g_atom env b)
  | Closure.Eq(a, b) -> Eq(g_atom env a, g_atom env b)
  | Closure.Get(a, b) -> Get(g_atom env a, g_atom env b)
  | Closure.Put(a, b, c) -> Put(g_atom env a, g_atom env b, g_atom env c)
  | Closure.Tuple(ts) -> Tuple(List.map (g_atom env) ts)
  | Closure.Unit -> Unit
  | Closure.Bool(b) -> Bool(b)
  | Closure.Int(n) -> Int(n)
  | Closure.Float(f) -> Float(f)
  | Closure.If (x, e1, e2) ->
      If (Var x, visit env e1, visit env e2)
  | Closure.Let ((x, t), e1, e2) ->
      let v, env' = add x t env (roots e2) in
      Let ((v, t), visit env e1, visit env' e2)
  | Closure.Var (x) ->
      Atom (g_atom env x)
  | Closure.MakeCls ((x,t),(clos),e) ->
      let r = roots e in
      let (x,env) = add x t env r in
      MakeCls ((x,t),
        { entry = clos.Closure.entry;
          actual_fv = List.map (g_atom env) clos.Closure.actual_fv
        },
        visit env e)
  | Closure.AppCls (x, idl) ->
      AppCls (g_atom env x, List.map (g_atom env) idl)
  | Closure.AppDir (x, idl) ->
      AppDir (x, List.map (g_atom env) idl)
  | Closure.LetTuple (idtl, x, e) ->
      let x = g_atom env x in
      let r = roots e in
      let rec loop env yl = function
        | [] ->
            LetTuple (List.rev yl, x, visit env e)
        | (id, t) :: rest ->
            let v, env' = add id t env r in
            loop env' ((v, t) :: yl) rest
      in loop env [] idtl
  | Closure.ExtArray (x, t) ->
      ExtArray (x, t)
  | Closure.ExtFunApp (x, idl, t) ->
      ExtFunApp (x, List.map (g_atom env) idl, t)

type fundef = { name : string * Type.t;
  args : (atom * Type.t) list;
  formal_fv : (atom * Type.t) list;
  body : t }

type prog = Prog of fundef list * t

let visit_fundef fundef =
  let r = roots fundef.Closure.body in
  (* (let string name, _ = fundef.Closure.name in
    Printf.eprintf "roots for %s: %s\n" name
      (String.concat " " (S.elements r))); *)
  let rec loop env yl = function
    | [] ->
        List.rev yl, env
    | (x, t) :: rest ->
        let v, env = add x t env r in
        loop env ((v, t) :: yl) rest
  in
  let name, _ = fundef.Closure.name in
  Printf.eprintf "Roots for %s: " name;
  let args, env = loop M.empty [] fundef.Closure.args in
  let formal_fv, env = loop env [] fundef.Closure.formal_fv in
  let body = visit env fundef.Closure.body in
  Printf.eprintf "\n";
  { name = fundef.Closure.name;
    args = args; formal_fv = formal_fv;
    body = body }

let apply (Closure.Prog(fundefs, e)) =
  Prog (List.map visit_fundef fundefs, visit M.empty e)
