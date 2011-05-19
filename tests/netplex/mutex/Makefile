build: server.run

test:
	./server.run

server: server.ml
	ocamlfind ocamlopt -o server -package netplex -linkpkg server.ml

server.run: server.ml
	ocamlfind ocamlc -o server.run -package netplex -linkpkg server.ml

.PHONY: clean
clean:
	rm -f *.cmx *.cmi *.o server server.run
	rm -rf sockdir
