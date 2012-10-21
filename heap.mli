type value =
    Int of int
  | Str of string
  | Bool of bool
  | Closure of Ast.expr * env
  | Cons of value * value
  | Nil
  | Undef
and binding = Ast.id * value ref
and env = binding list

(* Looks up a value in the environment. None if not found. *)
val lookup : Ast.id -> env -> value option

(* Updates a binding in the environment to a new value,
 * as a side effect. *)
val update : Ast.id -> value -> env -> unit

(* Creates a new binding in the environment with the given value. *)
val bind : Ast.id -> value -> env -> env

val value_to_string : value -> string
