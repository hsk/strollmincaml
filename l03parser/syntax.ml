type t =
  | Int of int
  | Add of t * t
  | Sub of t * t
  | Let of t * t
  | Print of t
