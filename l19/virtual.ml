open Format
open Utils

type r =
  | RL of Type.t * string
  | RN of Type.t * string
  | RG of Type.t * string

type t =
  | Call of bool * r * r * r list
  | Bin of r * string * r * r
  | Ret of r
  | InsertValue of r * r * r * int
  | ExtractValue of r * r * int
  (* 条件分岐 *)
  | Jne of r * string * string * string
  (* ジャンプ命令 *)
  | Goto of string * string
  (* ラベル命令 *)
  | Label of string * string
  (* SSA最適化のφ of ファイ 複数基本ブロックの変数を１つに合流させる *)
  | Phi of r * string * string * Type.t * r * r
  | Field of r * r * r
  | Load of r * r
  | Store of r * r

type fundef =
   {name : string; args : (Gc.atom * Type.t) list; body : t list; ret : Type.t}

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

let atomid atom =
  match atom with
  | Gc.Var(id) -> id
  | Gc.Root(id) -> id

let find x env =
  M.find (atomid x) env

let bin env op x y =
  let rx = find x env in
  let r = RL(regt rx, genid("..")) in
  add(Bin(r, op, rx, find y env));
  r

let rec visit (env:r M.t)(c: Gc.t): r =
  match c with
    | Gc.Int(i) ->
      RN(Type.Int, string_of_int i)
    | Gc.Add(x, y) -> bin env "add" x y
    | Gc.Sub(x, y) -> bin env "sub" x y
    | Gc.Neg(aId) ->
      let aR = find aId env in
      let bR = RN(Type.Int, "0") in
      let retR = RL(regt aR,genid("..")) in
      add(Bin(retR, "sub", aR, bR));
      retR
    | Gc.Float(f) ->
      RN(Type.Float, string_of_float f)
    | Gc.FAdd(x, y) -> bin env "fadd" x y 
    | Gc.FSub(x, y) -> bin env "fsub" x y
    | Gc.FMul(x, y) -> bin env "fmul" x y
    | Gc.FDiv(x, y) -> bin env "fdiv" x y
    | Gc.FNeg(aId) ->
      let aR = find aId env in
      let bR = RN(Type.Float, "0.0") in
      let retR = RL(regt aR,genid("..")) in
      add(Bin(retR, "fsub", aR, bR));
      retR
    | Gc.Bool(b) ->
      RN(Type.Bool, if b then "-1" else "0")
    | Gc.Eq(x, y) -> bin env "eq" x y
    | Gc.LE(x, y) -> bin env "le" x y
    | Gc.If(x, e1, e2) ->
      let id1 = genid("ok") in
      let (id2, l1) = (genid("else"), genid("else")) in
      let (id3, l2) = (genid("endif"), genid("endif")) in
      let rx = find x env in (* cond *)
      add(Jne(rx, id1, id1, id2));
      let r1 = visit env e1 in
      add(Label(l1, l1));
      add(Goto(id2, id3));
      let r2 = visit env e2 in
      add(Label(l2, l2));
      add(Goto(id3, id3));
      if ((regt r1) <> Type.Unit && (regt r1) = (regt r2))
      then (
        let r3 = RL(regt r1, genid("..")) in
        add(Phi(r3, l1, l2, regt r1, r1, r2));
        r3
      ) else
        RN(Type.Unit, "0")
    | Gc.Let((aId,aT), bK, cK) ->
      let bR = visit env bK in
      visit (M.add (atomid aId) bR env) (cK)
    | Gc.Unit -> RN(Type.Unit, "0")
    | Gc.Atom a ->
      (try
        find a env
      with
        Not_found ->
          failwith ("not found "^atomid a)
      )
    | Gc.AppDir(nameId, prmIds) ->
      app_dir env false nameId prmIds
    | Gc.ExtFunApp(nameId, prmIds, t) ->
      let prmRs = List.map (fun prmId -> find prmId env) prmIds in
      let nameR = RG(t, nameId) in
      let retR = RL(t, genid("..")) in
      add(Call(false, retR, nameR, prmRs));
      retR
    (* クロージャ生成 *)
    | Gc.MakeCls(
      (nameId, Type.Fun(funParamTs, funRetT)),
      { Gc.entry=ent; 
      Gc.actual_fv=freeVarIds}, funBodyC) ->
      (* 自由変数の型 *)
      let freeVarTs = List.map (fun s -> regt (find s env)) freeVarIds in
      (* 関数の型 *)
      let funT = Type.Fun(freeVarTs @ funParamTs, funRetT) in
      (* クロージャ用の構造体の素レジスタ *)
      let srcClosureR = RN(Type.Tuple(funT::freeVarTs),"undef") in
      (* クロージャ用の構造体 *)
      let closureR = RL(regt srcClosureR, genid("..")) in

      (* 構造体の0番目にfunBodyRを設定した構造体を作成 *)
      add(InsertValue(closureR, srcClosureR, RG(funT, ent) , 0));

      (* ループして1番目以降に自由変数をクロージャ構造体に保存する *)
      let (closureR_, _) = List.fold_left
        (fun (closureR, closureIndex) freeVarId ->
          let closureR_ = RL(regt closureR, genid("..")) in
          let freeVarR = find freeVarId env in
          add(InsertValue(closureR_, closureR, freeVarR, closureIndex));
          (closureR_, closureIndex + 1)
        )
        (closureR, 1)
        freeVarIds
      in
      (* 継続をコンパイル *)
      visit (M.add (atomid nameId) closureR_ env) funBodyC

    | Gc.MakeCls(_, _, _) -> assert false

    | Gc.AppCls(nameId, prmIds) ->
      app_cls env false nameId prmIds

    | Gc.Get(x, y) ->
      let xr = find x env in
      let xt = regt xr in
      (match xt with
        | Type.Array(Type.Unit) -> RN(Type.Unit, "0")
        | Type.Array(t) ->
          let reg5 = RL(t, genid("..")) in
          let reg4 = RL(Type.Array(t), genid("..")) in
          add(Field(reg4, xr, find y env));
          add(Load(reg5, reg4));
          reg5
        | t ->
          fprintf str_formatter "x=%s t=%a" (atomid x) Type.print_t t;
          failwith (flush_str_formatter())
      )
    | Gc.Put(x, y, z) ->
      let xr = find x env in
      let xt = regt xr in
      (match xt with
        | Type.Array(Type.Unit) -> RN(Type.Unit, "0")
        | Type.Array(t)  ->

          let reg4 = RL(Type.Array(t), genid("..")) in
          add(Field(reg4, xr, find y env));
          let reg5 = find z env in
          add(Store(reg5, reg4));
          reg5
        | t ->
          fprintf str_formatter "x=%s t=%a" (atomid x) Type.print_t t;
          failwith (flush_str_formatter())
      )
    | Gc.ExtArray(x, t) -> RG(t, "min_caml_" ^ x)
    | Gc.LetTuple (atl, a, e) ->
      let env = let_tuple env false atl a in
      visit env e
    | Gc.Tuple (xs:Gc.atom list) ->
      let ts = List.map (fun x -> regt (find x env)) xs in
      let t = Type.Tuple(ts) in

      let (r,_) = List.fold_left
        (fun ((src:r),(n:int)) (x:Gc.atom) ->
          (* 構造体 *)
          let dst = RL(t, genid("..")) in
          let r = find x env in
          add(InsertValue(dst, src, r, n));
          (dst,n+1)
        )
        (RN(t,"undef"), 0)
        xs
      in r

and app_dir env tail nameId prmIds =
      (try
        let prmRs = List.map (fun prmId -> find prmId env ) prmIds in
        let nameR = M.find nameId env in
        let retR = RL(regt nameR, genid("..")) in
        add(Call(tail, retR, nameR, prmRs));
        retR
      with
        Not_found ->
          failwith ("not found appdir "^ nameId)
      )
and app_cls env tail closureId prmIds =
    (* クロージャ実行 *)
      let funPrmRs = List.map
        (fun prmId -> find prmId env )
        prmIds
      in
      let closureR = find closureId env in

      let (funR, closurePrmRs, retT) =
        match regt closureR with
        | Type.Tuple((Type.Fun(_, retT) as funT)::prmTs) -> 
          let funR = RL(funT, genid("..")) in
          add(ExtractValue(funR, closureR, 0));
          let (closurePrmRs,_) = List.fold_left
            ( fun (closurePrmRs, closureIndex) closurePrmT ->
              let closurePrmR = RL(closurePrmT, genid("..")) in
              add(ExtractValue(closurePrmR, closureR, closureIndex));
              (closurePrmR::closurePrmRs, closureIndex + 1)
            )
            ([],1)
            prmTs
          in
          (funR, List.rev closurePrmRs, retT)
        | _ ->
          fprintf str_formatter "error id=%s t=%a" (atomid closureId) Type.print_t (regt closureR);
          failwith(flush_str_formatter())
      in

      let retR = RL(retT, genid("..")) in
      add(Call(tail, retR, funR, closurePrmRs @ funPrmRs));
      retR

and let_tuple env tail atl a =
        let ar = find a env in
        let (env,_ ) = List.fold_left
          (fun (env,i) (id1,t) ->
            let id1 = atomid id1 in
            let r = RL(t,id1) in
            add(ExtractValue(r, ar, i));
            (M.add id1 r env, i+1)
          )
          (env, 0)
          atl
        in
        env

let rec visit_tail env e =
  match e with
  | Gc.If(x, e1, e2) ->
    let id1 = genid("ok") in
    let id2 = genid("else") in
    let rx = find x env in (* cond *)
    add(Jne(rx, id1, id1, id2));
    let r1 = visit_tail env e1 in
    add(Label(id2, id2));
    let r2 = visit_tail env e2 in
    if ((regt r1) <> Type.Unit && (regt r1) = (regt r2))
    then r1
    else RN(Type.Unit, "0")

  | Gc.Let ((id, _), e1, e2) ->
      let v = visit env e1 in
      visit_tail (M.add (atomid id) v env) e2
  | Gc.AppDir(nameId, prmIds) ->
    let retR = app_dir env true nameId prmIds in
    add(Ret(retR));
    retR
  (* クロージャ実行 *)
  | Gc.AppCls(nameId, prmIds) ->
    let retR = app_cls env true nameId prmIds in
    add(Ret(retR));
    retR
  | Gc.LetTuple (atl, a, e) ->
    let env = let_tuple env true atl a in
    visit_tail env e
  | _ ->
    let v = visit env e in
    add(Ret(v));
    v

let visitfun env {
  Gc.name = (x, t); 
  Gc.args = yts;
  Gc.formal_fv = zts;
  Gc.body = e } =
  vs := [];
  match t with
  | Type.Fun(_, t) ->
    let env' = M.add x (RG(t,x)) env in
    let env' = M.add_list (List.map (fun (s,t) -> (atomid s, RL(t,atomid s))) zts) env' in
    let env' = M.add_list (List.map (fun (s,t) -> (atomid s, RL(t,atomid s))) yts) env' in
    let r = visit_tail env' e in
    (M.add x (RG((regt r),x)) env, { name = x; args = yts @ zts; body = List.rev !vs; ret = regt r })
  | _ -> assert false

let apply (Gc.Prog(fundefs, e)): prog =
  let fundefs = fundefs @ [{Gc.name=("main", Type.Fun([], Type.Unit));
    Gc.args=[]; Gc.formal_fv=[]; Gc.body= e}] in
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
