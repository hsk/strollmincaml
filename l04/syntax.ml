type t =
  | Int of int
  | Add of t * t
  | Sub of t * t
  | Let of (string * Type.t) * t * t
  | Print of t
  | Unit
  | Var of string
  