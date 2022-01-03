command: 
	@ocamlc -c A6.ml
	@ocamllex lexer.mll
	@ocamlyacc yaccer.mly
	@ocamlc -c yaccer.mli
	@ocamlc -c lexer.ml
	@ocamlc -c yaccer.ml
	@ocamlc -c main.ml
	@ocamlc -o LogPro A6.cmo  lexer.cmo yaccer.cmo main.cmo 
