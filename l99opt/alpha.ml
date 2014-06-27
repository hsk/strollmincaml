(* rename identifiers to make them unique (alpha-conversion) *)

open KNormal
open Utils

let find x env = try M.find x env with Not_found -> x

let genid x = genid ".."
let rec g env = function (* α変換ルーチン本体 (caml2html: alpha_g) *)
  | Unit -> Unit
  | Int(i) -> Int(i)
  | Bool(b) -> Bool(b)
  | Print(x) -> Print(find x env)
  | Float(d) -> Float(d)
  | Neg(x) -> Neg(find x env)
  | Add(x, y) -> Add(find x env, find y env)
  | Sub(x, y) -> Sub(find x env, find y env)
  | FNeg(x) -> FNeg(find x env)
  | FAdd(x, y) -> FAdd(find x env, find y env)
  | FSub(x, y) -> FSub(find x env, find y env)
  | FMul(x, y) -> FMul(find x env, find y env)
  | FDiv(x, y) -> FDiv(find x env, find y env)
  | If(x, e1, e2) -> If(find x env, g env e1, g env e2)
  | LE(x, y) -> LE(find x env, find y env)
  | Eq(x, y) -> Eq(find x env, find y env)
  | Let((x, t), e1, e2) -> (* letのα変換 (caml2html: alpha_let) *)
      let x' = genid x in
      Let((x', t), g env e1, g (M.add x x' env) e2)
  | Var(x) -> Var(find x env)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* let recのα変換 (caml2html: alpha_letrec) *)
      let env = M.add x (genid x) env in
      let ys = List.map fst yts in
      let env' = M.add_list2 ys (List.map genid ys) env in
      LetRec({ name = (find x env, t);
         args = List.map (fun (y, t) -> (find y env', t)) yts;
         body = g env' e1 },
       g env e2)
  | App(x, ys) -> App(find x env, List.map (fun y -> find y env) ys)
  | Tuple(xs) -> Tuple(List.map (fun x -> find x env) xs)
  | LetTuple(xts, y, e) -> (* LetTupleのα変換 (caml2html: alpha_lettuple) *)
      let xs = List.map fst xts in
      let env' = M.add_list2 xs (List.map genid xs) env in
      LetTuple(List.map (fun (x, t) -> (find x env', t)) xts,
         find y env,
         g env' e)
  | Get(x, y) -> Get(find x env, find y env)
  | Put(x, y, z) -> Put(find x env, find y env, find z env)
  (* | ExtArray(x) -> ExtArray(x) *)
  | ExtFunApp(x, ys) -> ExtFunApp(x, List.map (fun y -> find y env) ys)

let apply = g M.empty
