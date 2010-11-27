all: create_join

OCAMLC=ocamlc
# set to ocamlopt to get native executables

clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.cmxa *.o *.a 
	rm -f create_join

create_join: create_join.ml
	ocamlfind $(OCAMLC) -o create_join -package netmulticore -linkpkg -g \
	  create_join.ml
