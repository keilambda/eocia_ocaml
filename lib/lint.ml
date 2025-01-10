type op =
  | Read
  | Neg
  | Add
  | Sub

type expr =
  | Lit of int
  | Prim of op * expr list

let read = Prim (Read, [])

let neg e = Prim (Neg, [e])

let add a b = Prim (Add, [a; b])

let sub a b = Prim (Sub, [a; b])

let string_of_op = function
  | Read -> "read"
  | Neg -> "-"
  | Add -> "+"
  | Sub -> "-"

let rec string_of_expr = function
  | Lit n -> string_of_int n
  | Prim (op, es) -> "(" ^ string_of_op op ^ " " ^ String.concat " " (List.map string_of_expr es) ^ ")"

let leaf = function
  | Lit _ -> true
  | Prim (Read, []) -> false
  | Prim (Neg, [_]) -> false
  | Prim (Add, [_; _]) -> false
  | Prim (Sub, [_; _]) -> false
  | _ -> invalid_arg "bad special form"

let rec is_expr = function
  | Lit _ -> true
  | Prim (Read, []) -> true
  | Prim (Neg, [a]) -> is_expr a
  | Prim (Add, [a; b]) -> is_expr a && is_expr b
  | Prim (Sub, [a; b]) -> is_expr a && is_expr b
  | _ -> false

let rec interp_expr = function
  | Lit n -> n
  | Prim (Read, []) -> read_int ()
  | Prim (Neg, [a]) -> Int.neg (interp_expr a)
  | Prim (Add, [a; b]) -> interp_expr a + interp_expr b
  | Prim (Sub, [a; b]) -> interp_expr a - interp_expr b
  | _ -> invalid_arg "bad special form"

let pe_neg = function
  | Lit n -> Lit (Int.neg n)
  | other -> other

let pe_add a b = match (a, b) with
  | (Lit x, Lit y) -> Lit (x + y)
  | _ -> add a b

let pe_sub a b = match (a, b) with
  | (Lit x, Lit y) -> Lit (x - y)
  | _ -> sub a b

let rec pe_expr = function
  | Lit n -> Lit n
  | Prim (Read, []) -> read
  | Prim (Neg, [a]) -> pe_neg (pe_expr a)
  | Prim (Add, [a; b]) -> pe_add (pe_expr a) (pe_expr b)
  | Prim (Sub, [a; b]) -> pe_sub (pe_expr a) (pe_expr b)
  | _ -> invalid_arg "bad special form"
