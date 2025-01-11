type op = Read | Neg | Add | Sub
type expr = Lit of int | Prim of op * expr list

val read : expr
val neg : expr -> expr
val add : expr -> expr -> expr
val sub : expr -> expr -> expr

val string_of_op : op -> string
val string_of_expr : expr -> string

val leaf : expr -> bool
val is_expr : expr -> bool

val interp_expr : expr -> int

val pe_neg : expr -> expr
val pe_add : expr -> expr -> expr
val pe_sub : expr -> expr -> expr
val pe_expr : expr -> expr
