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
  | InsertValue of r * r * r * int
  | ExtractValue of r * r * int
  | Ret of r
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
    | Closure.Neg(aId) ->
      let aR = M.find aId env in
      let bR = RN(Type.Int, "0") in
      let retR = RL(regt aR,genid("..")) in
      add(Bin(retR, "sub", aR, bR));
      retR
    | Closure.Float(f) ->
      RN(Type.Float, string_of_float f)
    | Closure.FAdd(x, y) -> bin env "fadd" x y 
    | Closure.FSub(x, y) -> bin env "fsub" x y
    | Closure.FMul(x, y) -> bin env "fmul" x y
    | Closure.FDiv(x, y) -> bin env "fdiv" x y
    | Closure.FNeg(aId) ->
      let aR = M.find aId env in
      let bR = RN(Type.Float, "0.0") in
      let retR = RL(regt aR,genid("..")) in
      add(Bin(retR, "fsub", aR, bR));
      retR
    | Closure.Bool(b) ->
      RN(Type.Bool, if b then "-1" else "0")
    | Closure.Eq(x, y) -> bin env "eq" x y
    | Closure.LE(x, y) -> bin env "le" x y
    | Closure.If(x, e1, e2) ->
      let id1 = genid("ok") in
      let (id2, l1) = (genid("else"), genid("else")) in
      let (id3, l2) = (genid("endif"), genid("endif")) in
      let rx = M.find x env in (* cond *)
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
    | Closure.Let((aId,aT), bK, cK) ->
      let bR = visit env bK in
      visit (M.add aId bR env) (cK)
    | Closure.Print(aId) ->
      add(Print(M.find aId env));
      RN(Type.Unit,"0")
    | Closure.Unit -> RN(Type.Unit, "0")
    | Closure.Var a ->
      (try
        M.find a env
      with
        Not_found ->
          failwith ("not found "^ a)
      )
    | Closure.AppDir(nameId, prmIds) ->
      (try
        let prmRs = List.map (fun prmId -> M.find prmId env ) prmIds in
        let nameR = M.find nameId env in
        let retR = RL(regt nameR, genid("..")) in
        add(Call(retR, nameR, prmRs));
        retR
      with
        Not_found ->
          failwith ("not found appdir "^ nameId)
      )

    (* クロージャ生成 *)
    | Closure.MakeCls(
      (nameId, Type.Fun(funParamTs, funRetT)),
      { Closure.entry=ent; 
      Closure.actual_fv=freeVarIds}, funBodyC) ->
      (* 自由変数の型 *)
      let freeVarTs = List.map (fun s -> regt (M.find s env)) freeVarIds in
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
          let freeVarR = M.find freeVarId env in
          add(InsertValue(closureR_, closureR, freeVarR, closureIndex));
          (closureR_, closureIndex + 1)
        )
        (closureR, 1)
        freeVarIds
      in
      (* 継続をコンパイル *)
      visit (M.add nameId closureR_ env) funBodyC

    | Closure.MakeCls(_, _, _) -> assert false

    (* クロージャ実行 *)
    | Closure.AppCls(closureId, prmIds) ->
      let funPrmRs = List.map
        (fun prmId -> M.find prmId env )
        prmIds
      in
      let closureR = M.find closureId env in

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
          fprintf str_formatter "error id=%s t=%a" closureId Type.print_t (regt closureR);
          failwith(flush_str_formatter())
      in

      let retR = RL(retT, genid("..")) in
      add(Call(retR, funR, closurePrmRs @ funPrmRs));
      retR
    | Closure.Get(x, y) ->
      let xr = M.find x env in
      let xt = regt xr in
      (match xt with
        | Type.Array(Type.Unit) -> RN(Type.Unit, "0")
        | Type.Array(t) ->
          let reg5 = RL(t, genid("..")) in
          let reg4 = RL(Type.Array(t), genid("..")) in
          add(Field(reg4, xr, M.find y env));
          add(Load(reg5, reg4));
          reg5
        | t ->
          fprintf str_formatter "x=%s t=%a" x Type.print_t t;
          failwith (flush_str_formatter())
      )
    | Closure.Put(x, y, z) ->
      let xr = M.find x env in
      let xt = regt xr in
      (match xt with
        | Type.Array(Type.Unit) -> RN(Type.Unit, "0")
        | Type.Array(t)  ->

          let reg4 = RL(Type.Array(t), genid("..")) in
          add(Field(reg4, xr, M.find y env));
          let reg5 = M.find z env in
          add(Store(reg5, reg4));
          reg5
        | t ->
          fprintf str_formatter "x=%s t=%a" x Type.print_t t;
          failwith (flush_str_formatter())
      )
    | Closure.ExtFunApp(nameId, prmIds) ->
      let prmRs = List.map (fun prmId -> M.find prmId env) prmIds in
      let nameR = M.find nameId env in
      (match regt nameR with
        | Type.Fun(l,t) ->
          let retR = RL(t, genid("..")) in
          add(Call(retR, nameR, prmRs));
          retR
        | t -> failwith("type error")
      )
    | Closure.LetTuple (atl, a, e) ->
        let ar = M.find a env in
        let (env,_ ) = List.fold_left
          (fun (env,i) (id1,t) ->
            let r = RL(t,id1) in
            add(ExtractValue(r, ar, i));
            (M.add id1 r env, i+1)
          )
          (env, 0)
          atl
        in
        visit env e
    | Closure.Tuple (xs:string list) ->
      let ts = List.map (fun x -> regt (M.find x env)) xs in
      let t = Type.Tuple(ts) in

      let (r,_) = List.fold_left
        (fun ((src:r),(n:int)) (x:string) ->
          (* 構造体 *)
          let dst = RL(t, genid("..")) in
          let r = M.find x env in
          add(InsertValue(dst, src, r, n));
          (dst,n+1)
        )
        (RN(t,"undef"), 0)
        xs
      in r

let visitfun env {
    Closure.name = (x, t); 
    Closure.args = yts;
    Closure.formal_fv = zts;
    Closure.body = e } =
  vs := [];
  match t with
  | Type.Fun(_, t) ->
    let env' = M.add x (RG(t,x)) env in
    let env' = M.add_list (List.map (fun (s,t) -> (s, RL(t,s))) zts) env' in
    let env' = M.add_list (List.map (fun (s,t) -> (s, RL(t,s))) yts) env' in
    let r = visit env' e in
    add(Ret(r));
    (M.add x (RG((regt r),x)) env, { name = x; args = yts @ zts; body = List.rev !vs; ret = regt r })
  | _ -> assert false

let apply (Closure.Prog(fundefs, e)): prog =
  let fundefs = fundefs @ [{Closure.name=("main", Type.Fun([], Type.Unit));
    Closure.args=[]; Closure.formal_fv=[]; Closure.body= e}] in
  let env = M.add
    "create_array"
    (RG(Type.Fun([Type.Int;Type.Int],Type.Array(Type.Int)),"create_array"))
    M.empty
  in
  let (_,fundefs) =
    List.fold_left
      (fun  (env, fundefs) fundef ->
        let (env, fundef) = visitfun env fundef in
        (env, fundef::fundefs)
      )
      (env, [])
      fundefs
  in
  Prog(fundefs)
