open Ast
open Heap
open Util

let rec eval (ast : expr) (env : env) : value =
  match ast with
  | Int_e n -> Int n
  | Str_e s -> Str s
  | Bool_e b -> Bool b
  | Def_e _ -> runtime "define may occur at top level only"
  | Defrec_e _ -> runtime "definerec may occur at top level only"
  | Nil_e -> Nil
  | Id_e id -> (match (lookup id env) with 
      | None -> Nil 
      | Some v -> v)
  | Cons_e (x, y) -> Cons ((eval x env),(eval y env))
  | Let_e (x, e1, e2) -> let newenv=(bind x (eval e1 env) env) in 
    (eval e2 newenv)
  | Letrec_e (x, e1, e2) ->  let newenv = (bind x (Undef) env) in 
     update x (eval e1 newenv) newenv; (eval e2 newenv)
  | If_e (b, e1, e2) -> (match (eval b env) with
     | Bool bee -> if (bee) then (eval e1 env) else (eval e2 env)
     | _ -> runtime "No bool case matched for if")
  | Apply_e (e1, es) -> let foo acc ele = apply acc (eval ele env) in 
          let res = List.fold_left foo (eval e1 env) es in 
          (match res with
            | Closure(Fun_e(xs,e),env) -> 
                (match xs with 
                 | [] -> eval e env 
                 | _ -> res)
            | _ -> runtime "No closure match 2")
  | Fun_e (xs, e) ->
      Closure (Fun_e(xs,e),env)
  | Binop_e (op, e1, e2) -> apply_binop op (eval e1 env) (eval e2 env)
  | Unop_e (op, e) -> apply_unop op (eval e env)
  | Delayed_e (ex) -> Closure (ex,env);
  | Forced_e (del_expr) -> let res =  (eval del_expr env) in match res with
      | Closure(a,b) -> eval a b
      | _ -> res

and apply (f : value) (v : value) : value =
 (match f with
   | Closure(Fun_e(xs,e),env) -> (match xs with
       | [] -> eval e env
       | idhd::idtl -> 
         let newenv = bind idhd v env in Closure(Fun_e(idtl,e),newenv) )
   | _ -> runtime "No closure match")


and apply_binop (op : op) (v1 : value) (v2 : value) : value =
  match op with
    Plus ->
      (match (v1, v2) with (Int m, Int n) -> Int (m + n)
        | _ -> runtime "applying + to non-integer")
  | Minus ->
      (match (v1, v2) with (Int m, Int n) -> Int (m - n)
        | _ -> runtime "applying - to non-integer")
  | Mul ->
      (match (v1, v2) with (Int m, Int n) -> Int (m * n)
        | _ -> runtime "applying * to non-integer")
  | Div ->
      (match (v1, v2) with (Int m, Int n) -> Int (m / n)
        | _ -> runtime "applying / to non-integer")
  | Mod ->
      (match (v1, v2) with (Int m, Int n) -> Int (m mod n)
        | _ -> runtime "applying % to non-integer")
  | Eq ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m = n)
      | (Str m, Str n) -> Bool (m = n)
      | (Bool m, Bool n) -> Bool (m = n)
      | _ -> runtime "inappropriate comparison with =")
  | Neq ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m <> n)
      | (Str m, Str n) -> Bool (m <> n)
      | (Bool m, Bool n) -> Bool (m <> n)
      | _ -> runtime "inappropriate comparison with !=")
  | Lt ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m < n)
      | (Str m, Str n) -> Bool (m < n)
      | (Bool m, Bool n) -> Bool (m < n)
      | _ -> runtime "inappropriate comparison with <")
  | Leq ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m <= n)
      | (Str m, Str n) -> Bool (m <= n)
      | (Bool m, Bool n) -> Bool (m <= n)
      | _ -> runtime "inappropriate comparison with <=")
  | Gt ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m > n)
      | (Str m, Str n) -> Bool (m > n)
      | (Bool m, Bool n) -> Bool (m > n)
      | _ -> runtime "inappropriate comparison with >")
  | Geq ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m >= n)
      | (Str m, Str n) -> Bool (m >= n)
      | (Bool m, Bool n) -> Bool (m >= n)
      | _ -> runtime "inappropriate comparison with >=")
  | And ->
      (match (v1, v2) with
        (Bool m, Bool n) -> Bool (m && n)
      | _ -> runtime "applying & to non-boolean")
  | Or ->
      (match (v1, v2) with
        (Bool m, Bool n) -> Bool (m || n)
      | _ -> runtime "applying | to non-boolean")
  | _ -> runtime "not a binary operator"

and apply_unop (op : op) (v : value) : value =
  match op with
  | Minus ->
      (match v with Int n -> Int (-n) | _ ->
         runtime "applying - to non-integer")
  | Not ->
      (match v with Bool b -> Bool (not b) | _ ->
         runtime "applying ~ to non-boolean")
  | Car ->
      (match v with Cons (x, y) -> x | _ ->
         runtime "inappropriate argument for car")
  | Cdr ->
      (match v with Cons (x, y) -> y | _ ->
         runtime "inappropriate argument for cdr")
  | Null ->
      (match v with Cons (x, y) -> Bool false
       | Nil -> Bool true
       | _ -> runtime "inappropriate argument for null")
  | Load -> runtime "load may only occur at top level"
  | _ -> runtime "not a unary operator"
