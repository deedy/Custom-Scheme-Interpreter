val parse : string -> Ast.expr list
val print_value : Heap.value -> unit
val read_console : unit -> string
val read_file : string -> string
val eval_one : Heap.env -> Ast.expr -> Heap.env
val eval_list : Heap.env -> Ast.expr list -> Heap.env
val repl : Heap.env -> unit
