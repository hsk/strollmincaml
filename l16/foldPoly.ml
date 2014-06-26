open Syntax
open Format

module SetT =
  Set.Make
    (struct
      type t = Type.t
      let compare t1 t2 = if t1 = t2 then 0 else -1
    end)

let rec hasQ(t:Type.t):bool =
  match t with
    | Type.Var({contents=Type.Link t}) -> hasQ t
    | Type.Var(_) | Type.QVar(_) -> true
    | Type.Fun(ts, t2) ->
      let l = List.exists  hasQ ts  in 
      l || hasQ t2 
    | Type.Array(t) -> hasQ t
    | Type.Tuple(ts) -> List.exists hasQ ts
    | Type.Float
    | Type.Int | Type.Unit | Type.Bool -> false
    

let rec mangling (id:string) (t:Type.t):string =
  id^"."^(match t with
    | Type.Bool -> "b"
    | Type.Float -> "f"
    | Type.Int -> "l"
    | Type.Unit -> "v"
    | Type.Fun(ts,t2) ->
      mangling "" t2 ^
      string_of_int(List.length ts) ^
      String.concat "" (List.map (mangling "") ts)
    | Type.Array(t) -> "p"^(mangling "" t)
    | Type.Tuple(ts) ->
      "t" ^ String.concat "" (List.map (mangling "") ts)
    | t ->
      fprintf std_formatter "error mangling type %s %a" id Type.print_t t;
      (*raise(Error(flush_str_formatter()))*)
      "p"
  )

let print_funs ppf (funs:SetT.t M.t) =
  let funs = M.bindings funs in
  List.iter
    (fun (s,set) ->
      let ls = SetT.elements set in
      fprintf ppf "  %s %a@." s Type.print_ts ls
    )
    funs

let mangle funs ((id:string), (t:Type.t)):string =
  let a = not (M.mem id !Typing.extenv) in
  if (not (hasQ t)) && (M.mem id funs) && a then (

    fprintf std_formatter "mangl %s %a@." id Type.print_t t;
    fprintf std_formatter "%a" print_funs funs;
    mangling id t
  ) else id


let rec visit env (funs:SetT.t M.t) (e:Syntax.t) :(SetT.t M.t * Syntax.t) =
  let conv1 e f =
    let (funs,e) = visit env funs e in
    (funs, f(e))
  in
  let conv2 e1 e2 f =
    let (funs,e2) = visit env funs e2 in
    let (funs,e1) = visit env funs e1 in
    (funs, f e1 e2)
  in
  let convs es f =
    let (funs,es) = List.fold_right
      (fun e (funs,es)  ->
        let (funs, e) = visit env funs e in
        (funs, e::es)
      )
      es
      (funs,[])
    in
    (funs, f es)
  in
  match e with
    | Int _ | Bool _ | Float _ | Unit -> (funs, e)
    | Neg e -> conv1 e (fun e -> Neg e)
    | Not e -> conv1 e (fun e -> Not e)
    | FNeg e -> conv1 e (fun e -> FNeg e)
    | Array(e1,e2) -> conv2 e1 e2 (fun e1 e2 -> Array(e1,e2))
    | Add (e1, e2) -> conv2 e1 e2 (fun e1 e2 -> Add(e1,e2))
    | Sub (e1, e2) -> conv2 e1 e2 (fun e1 e2 -> Sub(e1,e2))
    | FAdd(e1, e2) -> conv2 e1 e2 (fun e1 e2 -> FAdd(e1,e2))
    | FSub(e1, e2) -> conv2 e1 e2 (fun e1 e2 -> FSub(e1,e2))
    | FMul(e1, e2) -> conv2 e1 e2 (fun e1 e2 -> FMul(e1,e2))
    | FDiv(e1, e2) -> conv2 e1 e2 (fun e1 e2 -> FDiv(e1,e2))
    | Eq(e1, e2) -> conv2 e1 e2 (fun e1 e2 -> Eq(e1,e2))
    | LE(e1, e2) -> conv2 e1 e2 (fun e1 e2 -> LE(e1,e2))
    | Get(e1, e2) -> conv2 e1 e2 (fun e1 e2 -> Get(e1,e2))
    | If (e1, e2, e3) ->
      let (funs, e3) = visit env funs e3 in
      let (funs, e2) = visit env funs e2 in
      let (funs, e1) = visit env funs e1 in
      (funs, If(e1,e2,e3))
    | Put (e1, e2, e3) ->
      let (funs, e3) = visit env funs e3 in
      let (funs, e2) = visit env funs e2 in
      let (funs, e1) = visit env funs e1 in
      (funs, Put(e1,e2,e3))
    | Tuple ts -> convs ts (fun ts -> Tuple(ts))
    | LetTuple (xts, e1, e2) ->
      let (funs, e2) = visit env funs e2 in
      let (funs, e1) = visit env funs e1 in
      (funs, LetTuple(List.map (fun (x,t) -> (mangle funs (x,t), t)) xts, e1, e2))
    | Var(a,t) -> 
      let funs =
        (* 一般化された型の場合 *)
        if hasQ(t) then
          (* そのまま *)
          funs
        (* funsに型を追加 *)
        else if M.mem a funs
        then M.add a (SetT.add t (M.find a funs)) funs
        else M.add a (SetT.singleton t) funs
      in
      (* 名前はマングリングが必要ならマングリングして返す *)
      (funs, Var(mangle funs (a,t),t))
    | Let((a,t), b, c) ->
      (* cを先に変換 *)
      let (funs, c) = visit (M.add a t env) funs c in
      (* bを次に変換 *)
      let (funs, b) = visit env funs b in
      (* 名前はマングリングが必要な場合はマングリング *)
      (funs, Let((mangle funs (a, t),t), b, c))
    | App(a, xs) ->
      (* 引数を変換 *)
      let (funs, xs) = List.fold_right
        (fun x (funs,ls) ->
          let (funs,x) = visit env funs x in
          (funs, x::ls)
        )
        xs 
        (funs,[]) in
      (* 関数を変換 *)
      let (funs, a) = visit env funs a in
      (funs, App(a, xs))
    | LetRec({name=(id,t);args=args;body=e},k) ->

      (* 先に後続の式を変換し、関数のインスタンス化を取得 *)
      let ((funs:SetT.t M.t),(k:Syntax.t)) = visit (M.add id t env) funs k in

      if not (hasQ(t)) then
        (* 関数が一般化されてない場合 *)

        (* 関数の本体を変換 *)
        let (funs,e) = visit (M.add id t env) funs e  in
        (* 引数名は必ずマングリング *)
        let args = List.map (fun (s,t)->(mangling s t,t)) args in
        (* 変換結果を返却 *)
        (funs, LetRec({name=(mangle funs (id,t),t);args=args;body=e},k))
      else if not (M.mem id funs) then
        (* 関数が使われていない場合は、要らない *)
        (funs, k)
      else
      (* 型tで実体化 *)
      (* 一般化された関数の場合 *)
      let visitInst (t:Type.t) ((funs:SetT.t M.t),(k:Syntax.t)) =

        (* 関数の型からパラメータの型を取得 *)
        let ts = match t with
        | Type.Fun(ts,t) -> ts
        | _ ->
          fprintf str_formatter "type error %a" Type.print_t t;
          failwith (flush_str_formatter())
        in
        (* 型をインスタンス化されたものに変更したargsを求める *)
        let args = List.map2 (fun (s,_) t -> (s, t)) args ts in
        (* 変換本体から関数を作成 *)
        let f = LetRec({name=(mangling id t,t);args=args;body=e},Unit) in
        (* 関数のインスタンス化する *)
        let f = Typing.instE f in
        (* 型推論し直す *)

        let _ = Typing.visit (M.add id t env) f in
        (* 一般化しなおす *)
        let f = Typing.genE f in
        (* 関数本体を変換し直す(内部のジェネリックスを取得する) *)
        let (funs, f) = visit (M.add id t env) funs f in
        (* もう一回一般化して、関数をつくり直し *)
        let e = match f with
          | LetRec({name=n;args=args;body=e},Unit) ->
            LetRec({name=n;args=args;body=e},k)
          | _ -> assert false
        in
        
        (funs, e)
      in
      (* 対応する名前の関数の実体化でループして変換する *)

      List.fold_right
        visitInst
        (SetT.elements (M.find id funs))
        (funs,k)

let apply(e:Syntax.t):Syntax.t =
  let e = Typing.genE e in
  fprintf std_formatter "fold %a@." Syntax.print_t e;
  (* 多相関数を変換 *)
  let (_,e2) = visit M.empty M.empty e in
  e2
