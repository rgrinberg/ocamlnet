build: test_acceptor 

test_acceptor: test_acceptor.ml
	ocamlfind ocamlc -o test_acceptor -package "netstring,netcgi2,unix,nethttpd" \
		test_acceptor.ml -linkpkg -g

test:
	./test_acceptor

clean:
	rm -f *.cmi *.cmo *.cmx *.o
	rm -f test_acceptor 
