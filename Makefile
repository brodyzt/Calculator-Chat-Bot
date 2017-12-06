
OBJS=server.cmo
NAME=server
OFIND=ocamlfind ocamlc -thread -package cohttp.lwt,cohttp.async,lwt.ppx,yojson

$(NAME).byte: $(OBJS)
		$(OFIND) -linkpkg -o $@ $(OBJS) $(NAME).ml

%.cmo: %.ml
		$(OFIND) -c $<i
		$(OFIND) -c $<

install:
	opam install -y yojson lwt cohttp-lwt-unix cohttp ocurl ounit

test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

clean:
	ocamlbuild -clean
	rm -f *.cm*
	rm -f *.byte
	rm -f finalsrc.zip

repl:
	ocamlbuild -use-ocamlfind repl.byte && ./repl.byte

zip:
	zip finalsrc.zip *.ml*

compile:
	ocamlbuild -use-ocamlfind lexer.byte eval.cmo comb_eval.cmo mod_arith.cmo linear_alg.cmo rsa.cmo simpl_arith.cmo repl.byte

server:
	# ocamlfind ocamlc -thread \
	# -package cohttp.lwt,lwt.ppx,yojson,curl \
	# -linkpkg types.cmo types.ml lexer.cmo eval.cmo eval.ml server.cmo server.ml && \
	# ./server.byte
	ocamlbuild -use-ocamlfind -cflags '-package cohttp',\
	-package,cohttp.lwt,\
	-package,lwt.ppx,\
	-package,yojson,\
	-package,curl,\
	-linkpkg server.cmo server.ml server.byte && \
	./server.byte

