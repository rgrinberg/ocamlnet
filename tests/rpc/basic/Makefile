.PHONY: build clean test

build: test_rtypes

test_rtypes: test_rtypes.ml
	ocamlfind ocamlc -o test_rtypes -linkpkg \
		-package netstring \
		test_rtypes.ml


test:
	./test_rtypes

clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.cmxa *.a *.o
	rm -f test_rtypes
