type t =
| Int of int
| Add of t * t
| Sub of t * t

let rec eval t =
  match t with
  | Int i -> i
  | Add(x,y) -> (eval x) + (eval y)
  | Sub(x,y) -> (eval x) - (eval y)

let _ =
  let ast = Sub(Add(Int(1),Int(2)),Int(1)) in
  let a = eval ast in
  print_string ((string_of_int a) ^ "\n")
