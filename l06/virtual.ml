open Format
open Utils
type r =
  | RL of Type.t * string
  | RN of Type.t * string
  | RG of Type.t * string

let regid = function
  | RL (_,id) -> id
  | RN (_,id) -> id
  | RG (_,id) -> id

let regt = function
  | RL (t,_) -> t
  | RN (t,_) -> t
  | RG (t,_) -> t

type t =
  | Print of r
  | Bin of r * string * r * r
  | Call of r * r * r list
  | Ret of r

type fundef =
   {name : string; args : (string * Type.t) list; body : t list; ret : Type.t}

type prog = Prog of fundef list

let vs :t list ref = ref []

let add (v:t): unit =
  vs := v :: !vs

let bin env op x y =
  let rx = M.find x env in
  let r = RL(regt rx, genid("..")) in
  add(Bin(r, op, rx, M.find y env));
  r

let rec visit(env:r M.t)(c: Closure.t): r =
  match c with
    | Closure.Int(i) ->
      RN(Type.Int, string_of_int i)
    | Closure.Add(x, y) -> bin env "add" x y
    | Closure.Sub(x, y) -> bin env "sub" x y
    | Closure.Let((aId,aT), bK, cK) ->
      let bR = visit env bK in
      visit (M.add aId bR env) (cK)
    | Closure.Print(aId) ->
      add(Print(M.find aId env));
      RN(Type.Unit,"void")
    | Closure.Unit -> RN(Type.Unit, "void")
    | Closure.Var a ->
      M.find a env
    | Closure.AppDir(nameId, prmIds) ->
      let prmRs = List.map (fun prmId -> M.find prmId env ) prmIds in
      let nameR = M.find nameId env in
      let retR = RL(regt nameR, genid("..")) in
      add(Call(retR, nameR, prmRs));
      retR

let visitfun env {
    Closure.name = (x, t); 
    Closure.args = yts;
    Closure.body = e } =
  vs := [];
  match t with
  | Type.Fun(_, t) ->
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
      (fun  (env, fundefs) fundef ->
        let (env, fundef) = visitfun env fundef in
        (env, fundef::fundefs)
      )
      (M.empty, [])
      fundefs
  in
  Prog(fundefs)
