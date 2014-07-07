type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Int of int
  | Var of t option ref
  
let gentyp () = Var(ref None) (* 新しい型変数を作る *)
