all: create_join chameneos

OCAMLC=ocamlc
# set to ocamlopt to get native executables

clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.cmxa *.o *.a 
	rm -f create_join chameneos

create_join: create_join.ml
	ocamlfind $(OCAMLC) -o create_join -package netmulticore -linkpkg -g \
	  create_join.ml

chameneos: chameneos.ml
	ocamlfind $(OCAMLC) -o chameneos -package netmulticore,netcamlbox \
 	   -linkpkg -g \
	  chameneos.ml
