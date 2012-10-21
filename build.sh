sources="util.mli ast.mli Parser/parser.mli heap.mli eval.mli repl.mli \
util.ml ast.ml Parser/lexer.ml Parser/parser.ml heap.ml eval.ml repl.ml"

ocamlyacc -v Parser/parser.mly
ocamllex Parser/lexer.mll

ocamlc -o interpreter.exe -g -I Parser str.cma $sources
if [ $? -ne 0 ]; then
  echo 'WARNING: Your code did not compile.'
fi