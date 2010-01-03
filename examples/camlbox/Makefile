all: speed manymult unimult

clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.cmxa *.o *.a speed manymult unimult

speed: speed.ml
	ocamlfind ocamlopt -o speed -package netcamlbox -linkpkg speed.ml

manymult: manymult.ml
	ocamlfind ocamlopt -o manymult -package netcamlbox -linkpkg manymult.ml

unimult: unimult.ml
	ocamlfind ocamlopt -o unimult -package netcamlbox -linkpkg unimult.ml
