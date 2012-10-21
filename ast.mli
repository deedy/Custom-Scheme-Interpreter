type id = string
type op =
    Plus
  | Minus
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq
  | And
  | Or
  | Not
  | Car
  | Cdr
  | Null
  | Load
val op_to_string : op -> string
type expr =
    Int_e of int
  | Str_e of string
  | Bool_e of bool
  | Id_e of id
  | Nil_e
  | Cons_e of expr * expr
  | Let_e of id * expr * expr
  | Letrec_e of id * expr * expr
  | If_e of expr * expr * expr
  | Apply_e of expr * expr list
  | Fun_e of id list * expr
  | Def_e of id list * expr
  | Defrec_e of id list * expr
  | Binop_e of op * expr * expr
  | Unop_e of op * expr
  | Delayed_e of expr
  | Forced_e of expr
val to_string : expr -> id
