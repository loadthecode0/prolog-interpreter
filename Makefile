

.PHONY: clean

make:
	ocamlc -c ast.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c mgu.ml
	
	ocamlc -c interpreter.ml
	ocamlc -c main.ml
	ocamlfind ocamlc -linkpkg -package unix -o main ast.cmo parser.cmo lexer.cmo mgu.cmo  interpreter.cmo  main.cmo

clean:
	rm -f lexer.ml parser.ml parser.mli
	rm -f *.c*
	rm -f main

