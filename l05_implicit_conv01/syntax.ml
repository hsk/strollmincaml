type t =
  | Int of Type.t * int
  | Add of Type.t * t * t
  | Sub of Type.t * t * t
  | Let of (string * Type.t) * t * t
  | Unit
  | Var of string
  | Cast of Type.t * t
  | Print of t
