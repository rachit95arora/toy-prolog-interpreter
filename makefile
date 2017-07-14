all:
	ocamlc -c tree.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c toyprolog.ml
	ocamlc -o toyprolog tree.cmo  lexer.cmo parser.cmo toyprolog.cmo 
	
clean:
	rm toyprolog *.cmo *.cmi *.mli lexer.ml parser.ml
