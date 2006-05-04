.PHONY: all
all: add_sequential add_forking add_procpool

add_sequential: add_sequential.ml
	ocamlfind ocamlc -g -o add_sequential -package cgi -linkpkg add_sequential.ml

add_forking: add_forking.ml
	ocamlfind ocamlc -g -o add_forking -package cgi -linkpkg add_forking.ml

add_procpool: add_procpool.ml
	ocamlfind ocamlc -g -o add_procpool -package cgi -linkpkg add_procpool.ml

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cgi *.opt *.cmx *.o
	rm -f add_sequential add_forking add_procpool
