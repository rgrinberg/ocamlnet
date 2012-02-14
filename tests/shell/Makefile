.PHONY: build clean test

build: mass_creation

mass_creation: mass_creation.ml
	ocamlfind ocamlc -o mass_creation -linkpkg \
		-package shell \
		mass_creation.ml


test:
	./mass_creation

clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.cmxa *.a *.o
	rm -f mass_creation
