open Format
open Utils
open Syntax

type t =
  | Int of int
  | Add of string * string
  | Sub of string * string
  | Print of string
  | Let of (string * Type.t) * t * t
  | Unit
  | Var of string
  | LetRec of fundef * t
  | App of string * string list
and fundef = {
  name : string * Type.t;
  args : (string * Type.t) list;
  body : t }


let rec print_t ppf = function
  | Int i -> fprintf ppf "Int(%d)@?" i
  | Add(a,b) -> fprintf ppf "Add(\"%s\",\"%s\")@?" a b
  | Sub(a,b) -> fprintf ppf "Sub(\"%s\",\"%s\")@?" a b
  | Print(a) -> fprintf ppf "Print(\"%s\")@?" a
  | Let((s,t),a,b) -> fprintf ppf "Let((\"%s\",%a),%a,%a)@?" s Type.print_t t print_t a print_t b
  | Unit -> fprintf ppf "Unit@?"
  | Var(a) -> fprintf ppf "Var(\"%s\")@?" a
  | LetRec({name=(s,t);args=sts;body=a},b) ->
    fprintf ppf "LetRec({name=(\"%s\",%a);args=%a;body=%a},%a)@?"
      s Type.print_t t
      Syntax.print_sts sts
      print_t a print_t b
  | App(s,ss) -> fprintf ppf "App(\"%s\",[%s])@?" s (String.concat "; " ss)

let insert_let (e, t) k = (* letを挿入する補助関数 (caml2html: knormal_insert) *)
  match e with
  | Var(x) -> k x
  | _ ->
    let x = genid("..") in
    let e', t' = k x in
    Let((x, t), e, e'), t'

let rec visit(env:Type.t M.t)(e:Syntax.t):(t * Type.t) =
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
    | Syntax.Print(aE) ->
      insert_let (visit env aE) (fun x ->
        (Print x, Type.Unit)
      )
    | Syntax.Let((x,t), e1, e2) ->
      let e1', t1 = visit env e1 in
      let e2', t2 = visit (M.add x t env) e2 in
      Let((x, t), e1', e2'), t2
    | Syntax.Var(s) ->
      Var(s), (M.find s env)
    | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2) ->
      let env' = M.add x t env in
      let e2', t2 = visit env' e2 in
      let e1', t1 = visit (M.add_list yts env') e1 in
      LetRec({ name = (x, t); args = yts; body = e1' }, e2'), t2
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

let apply (e: Syntax.t): t =
  fst (visit M.empty e)
