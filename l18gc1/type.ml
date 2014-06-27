type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Int
  | Float
  | Bool
  | Fun of t list * t (* arguments are uncurried *)
  | Array of t
  | Tuple of t list
  | Var of tv ref
  | QVar of string
and tv =
  | Unbound of string * int
  | Link of t
  
let gentyp () = Var(ref (Unbound(Utils.genid("..t"),1000000)))
 (* 新しい型変数を作る *)

open Format

let rec prints f ppf ls =
  let rec loop ppf = function
    | [] -> ()
    | [l] -> f ppf l
    | l::ls -> fprintf ppf "%a; %a@?" f l loop ls
  in fprintf ppf "[%a]@?" loop ls

let rec print_t ppf = function
  | Unit -> fprintf ppf "Unit@?"
  | Int -> fprintf ppf "Int@?"
  | Float -> fprintf ppf "Float@?"
  | Bool -> fprintf ppf "Bool@?"
  | Var({contents=Unbound(s,i)}) -> fprintf ppf "Var(ref Unbound(\"%s\",%d))@?" s i
  | Var({contents=Link t}) -> fprintf ppf "Var(ref (Link(%a)))@?" print_t t
  | Fun(ts,t) -> fprintf ppf "Fun(%a,%a)@?" print_ts ts print_t t
  | Array(t) -> fprintf ppf "Array(%a)@?" print_t t
  | Tuple(ts) -> fprintf ppf "Tuple(%a)@?" print_ts ts
  | QVar(s) -> fprintf ppf "QVar(\"%s\")@?" s
and print_tv ppf = function
    | Unbound(s,i) -> Format.fprintf ppf "Unbound(\"%s\",%d)@?" s i
    | Link(t) -> Format.fprintf ppf "Link(%a)@?" print_t t
and print_ts ppf ts = prints print_t ppf ts
