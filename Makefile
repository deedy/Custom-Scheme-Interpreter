# PS4 makefile - in case you're into that kind of thing
#
# targets are:
#
# all -- rebuild the project (default)
# clean -- remove all objects and executables

# uncomment for Mac OSX
# export SHELL = /bin/bash
# export PATH = /usr/bin:/bin:/usr/local/bin

SOURCES = util.mli ast.mli Parser/parser.mli heap.mli eval.mli repl.mli \
util.ml ast.ml Parser/lexer.ml Parser/parser.ml heap.ml eval.ml repl.ml

.PHONY: all
all: interpreter.exe

.PHONY: clean
clean:
	rm -f interpreter.exe
	rm -f Parser/Parser.mli Parser/parser.ml Parser/lexer.ml
	for X in . Parser; do \
      for Y in cmo cmi output; do \
        rm -f $$X/*.$$Y; \
      done; \
    done

interpreter.exe: $(SOURCES)
	ocamlc -o interpreter.exe -g -I Parser str.cma $(SOURCES)

Parser/parser.mli Parser/parser.ml: Parser/parser.mly
	ocamlyacc -v Parser/parser.mly

Parser/lexer.ml: Parser/lexer.mll Parser/parser.ml
	ocamllex Parser/lexer.mll
