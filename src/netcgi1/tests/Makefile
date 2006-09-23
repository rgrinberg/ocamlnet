all: toploop browsertest.cgi jserv

toploop:
	ocamlfind ocamlmktop -o toploop -package unix,pcre,findlib -linkpkg -custom

browsertest.cgi: browsertest.ml
	ocamlfind ocamlc -o browsertest.cgi -package unix,pcre -linkpkg \
		-I .. -I ../../netstring \
		netstring.cma cgi.cma browsertest.ml

jserv: jserv.ml
	ocamlfind ocamlc -o jserv -package unix,pcre -linkpkg \
		-I .. -I ../../netstring \
		netstring.cma cgi.cma jserv.ml

jserv_mt: jserv_mt.ml
	ocamlfind ocamlc -o jserv_mt -thread -package unix,pcre,threads -linkpkg \
		-I .. -I ../../netstring \
		netstring.cma netstring_mt.cmo cgi.cma jserv_mt.ml

jserv_mt_opt: jserv_mt.ml
	ocamlfind ocamlopt -o jserv_mt -thread -package unix,pcre,threads -linkpkg \
		-I .. -I ../../netstring \
		netstring.cmxa netstring_mt.cmx cgi.cmxa jserv_mt.ml

test_netcgi: test_netcgi.ml
	ocamlfind ocamlc -o test_netcgi -package unix,pcre -I ../../netstring -I .. netstring.cma cgi.cma -linkpkg test_netcgi.ml -g
