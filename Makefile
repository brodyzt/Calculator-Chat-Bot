test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte
clean:
	ocamlbuild -clean
	rm -f finalsrc.zip
repl:
	ocamlbuild -use-ocamlfind repl.byte && ./repl.byte

zip:
	zip finalsrc.zip *.ml*
compile:
	ocamlbuild -use-ocamlfind lexer.byte eval.byte comb_eval.cmo mod_arith.cmo linear_alg.cmo rsa.cmo simpl_arith.cmo systems_eqs.cmo repl.byte