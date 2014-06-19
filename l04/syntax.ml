type t =
  | Int of int
  | Add of t * t
  | Sub of t * t
  | Print of t
  | Let of (string * Type.t) * t * t
  | Unit
  | Var of string
  