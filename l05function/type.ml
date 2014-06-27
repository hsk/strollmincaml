type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Int
  | Var of t option ref
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  
let gentyp () = Var(ref None) (* 新しい型変数を作る *)

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
  | Var({contents=None}) -> fprintf ppf "Var(ref None)@?"
  | Var({contents=Some t}) -> fprintf ppf "Var(ref (Some(%a)))@?" print_t t
  | Fun(ts,t) -> fprintf ppf "Fun(%a,%a)@?" print_ts ts print_t t
  | Tuple(ts) -> fprintf ppf "Tuple(%a)@?" print_ts ts
and print_ts ppf ts = prints print_t ppf ts
