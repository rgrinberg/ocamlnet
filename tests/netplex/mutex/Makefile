P=../../../src

server: server.ml
	env OCAMLPATH="$(P)" \
	ocamlfind ocamlopt -o server -package netplex -linkpkg server.ml

.PHONY: clean
clean:
	rm -f *.cmx *.cmi *.o server
	rm -rf sockdir
