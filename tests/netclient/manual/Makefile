all: testmt

testmt: 
	ocamlfind ocamlc -package netclient \
		-predicates mt_posix \
		-linkpkg -thread -custom testmt.ml \
		-o testmt


clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa
	rm -f testmt
