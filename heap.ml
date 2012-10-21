open Ast
open Util

type value = 
  | Int of int 
  | Str of string 
  | Bool of bool
  | Closure of expr * env
  | Cons of value * value 
  | Nil
  | Undef
and binding = id * value ref
and env = binding list

(* lookup a value in the environment *)
let lookup (x : id) (env : env) : value option =
 let filtlist = List.filter (fun y->let (id,value)=y in (id=x)) env in
  (match filtlist with 
   | [] -> None
   | hd::tl -> Some !(snd(hd)))


(* update binding x to value v in the environment *)
let update (x : id) (v : value) (env : env) : unit =
 let filtlist = List.filter (fun y->let (id,value)=y in (id=x)) env in 
  (match filtlist with 
   | [] -> runtime "updating nothing"
   | hd::tl -> snd(hd):=v)

(* create new binding, append to environment *)
let bind (x : id) (v : value) (env : env) : env =
 (x,ref v)::env


let rec value_to_string (x : value) : string =
  let rec cons_to_string (z : value) : string list =
    match z with
    | Nil -> [];
    | Cons (x, y) -> value_to_string x :: cons_to_string y
    | y -> ["."; value_to_string y]
  in
    match x with
    | Int n -> string_of_int n
    | Str s -> s
    | Bool b -> string_of_bool b
    | Closure (a,b) -> "<fun>"
    | Nil -> "()"
    | Cons  _ -> listify (cons_to_string x)
    | Undef -> failwith "Oppan"

