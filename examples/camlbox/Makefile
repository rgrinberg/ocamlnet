all: speed

clean:
	rm -f *.cmi *.cmo *.cma *.o *.a speed

speed: speed.ml
	ocamlfind ocamlopt -o speed -package netcamlbox -linkpkg speed.ml

