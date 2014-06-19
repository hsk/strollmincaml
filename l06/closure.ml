open Format

type closure = { entry : string; actual_fv : string list }
type t = (* クロージャ変換後の式 (caml2html: closure_t) *)
  | Unit
  | Int of int
  | Add of string * string
  | Sub of string * string
  | Let of (string * Type.t) * t * t
  | Var of string
  | AppDir of string * string list
  | Print of string
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
  | KNormal.Print(x) -> Print(x)

(**
 * クロージャ変換
 *)
let apply(e:KNormal.t): prog =
  toplevel := [];
  let e = visit e in
  Prog(List.rev !toplevel, e)
