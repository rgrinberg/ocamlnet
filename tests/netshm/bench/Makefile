build: lt1

test:
	./lt1

lt1: lt1.ml
	ocamlfind ocamlc -o lt1 -package netshm -linkpkg lt1.ml

run_lt1: lt1
	./lt1

clean:
	rm -f lt1 *.cmi *.cmo
