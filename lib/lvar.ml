open Lint

type expr =
  | Lit of int
  | Var of string
  | Let of string * expr * expr
  | Prim of op * expr list

let rec string_of_expr = function
  | Lit n -> string_of_int n
  | Var n -> n
  | Let (name, exp, body) -> "(let ([" ^ name ^ " " ^ string_of_expr exp ^ "]) " ^ string_of_expr body ^ ")"
  | Prim (op, es) -> "(" ^ string_of_op op ^ " " ^ String.concat " " (List.map string_of_expr es) ^ ")"

module Env = Map.Make(String)

let rec interp_expr env = function
  | Lit n -> n
  | Var n -> interp_expr env (Env.find n env)
  | Let (name, exp, body) -> interp_expr (Env.add name exp env) body
  | Prim (Read, []) -> read_int ()
  | Prim (Neg, [a]) -> Int.neg (interp_expr env a)
  | Prim (Add, [a; b]) -> (interp_expr env a) + (interp_expr env b)
  | Prim (Sub, [a; b]) -> (interp_expr env a) - (interp_expr env b)
  | Prim (_, _) -> invalid_arg "bad special form"
