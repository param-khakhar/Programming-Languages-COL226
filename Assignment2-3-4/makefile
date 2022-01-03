command: lexer.mll yaccer.mly
	@ocamlyacc yaccer.mly
	@ocamlc -c yaccer.mli
	@ocamllex lexer.mll
	@ocamlc -c lexer.ml
	@ocamlc -c yaccer.ml
	@ocamlc -c main.ml
	@ocamlc -o assignment4 str.cma lexer.cmo yaccer.cmo main.cmo
