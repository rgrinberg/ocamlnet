all: http_daemon

http_daemon: http_daemon.ml
	OCAMLPATH=../.. \
		ocamlfind ocamlc -o http_daemon \
			-package netplex,nethttpd -linkpkg \
			http_daemon.ml

http_daemon.opt: http_daemon.ml
	OCAMLPATH=../.. \
		ocamlfind ocamlopt -o http_daemon.opt \
			-package netplex,nethttpd -linkpkg \
			http_daemon.ml

clean:
	rm -f *.cmi *.cmo *.cma *.cmxa *.cmx *.o *.a
	rm -f http_daemon
