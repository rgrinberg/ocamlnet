all: test_acceptor 

test_acceptor: test_acceptor.ml ../nethttpd.cma
	ocamlfind ocamlc -o test_acceptor -package "netstring,netcgi2,unix" -I .. \
		nethttpd.cma test_acceptor.ml -linkpkg -g
clean:
	rm -f *.cmi *.cmo *.cmx *.o
	rm -f test_acceptor 
