all: hhamsters hhamsters.run

build: hhamsters.run

test:
	./hhamsters.run

hhamsters: hhamsters.ml
	ocamlfind ocamlopt -o hhamsters -g -package netclient -linkpkg hhamsters.ml

hhamsters.run: hhamsters.ml
	ocamlfind ocamlc -o hhamsters.run -g -package netclient -linkpkg hhamsters.ml


.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.o *.cmx *.a *.cmxa *.run
	rm -f hhamsters hhamsters.run
