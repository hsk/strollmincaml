type t =
  | Int of int
  | Add of t * t
  | Sub of t * t
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
and fundef = { name : string * Type.t; args : (string * Type.t) list; body : t }

open Format

let rec print_t ppf = function
  | Int(i) -> fprintf ppf "Int(%d)@?" i
  | Add(a,b) -> fprintf ppf "Add(%a,%a)@?" print_t a print_t b
  | Sub(a,b) -> fprintf ppf "Sub(%a,%a)@?" print_t a print_t b
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
and print_st ppf = function
  | (s,t) -> fprintf ppf "(\"%s\",%a)" s Type.print_t t
and print_sts ppf ls = Type.prints print_st ppf ls
and print_ts ppf ls = Type.prints print_t ppf ls
