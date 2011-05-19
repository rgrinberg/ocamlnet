# Run with:
# OCAMLPATH=../../../src/ CAML_LD_LIBRARY_PATH=../../../src/netsys make

STD_PACKAGES = unix pcre str findlib netstring

DEBUG  = -g
# Invoke with: make DEBUG=-g


test: test_server refuser test_client test_bad_request
	./test_bench

#run:
#	ocamlfind ocamlc -o run -make-runtime -package "$(STD_PACKAGES)" \
#		-linkpkg

test_server.cmo: test_server.ml
	ocamlfind ocamlc $(DEBUG) -package "$(STD_PACKAGES)" -c $<

test_server: test_server.cmo
	ocamlfind ocamlc -o test_server $(DEBUG) \
		-package "$(STD_PACKAGES)" -linkpkg \
		test_server.cmo

refuser.cmo: refuser.ml
	ocamlfind ocamlc $(DEBUG) -package "$(STD_PACKAGES)" -c $<

refuser: refuser.cmo
	ocamlfind ocamlc -o refuser $(DEBUG)  \
		-package "$(STD_PACKAGES)" -linkpkg \
		refuser.cmo

test_client.cmo: test_client.ml
	ocamlfind ocamlc $(DEBUG) -package "$(STD_PACKAGES)" \
		-I .. -c $<

test_client: test_client.cmo ../netclient.cma
	ocamlfind ocamlc -o test_client $(DEBUG) \
		-predicates debug -verbose \
		-package "$(STD_PACKAGES) equeue netstring" \
		-linkpkg ../netclient.cma test_client.cmo

test_bad_request: test_bad_request.ml ../netclient.cma
	ocamlfind ocamlc -o test_bad_request $(DEBUG) \
		-predicates debug \
		-package "$(STD_PACKAGES) equeue netstring" \
		-linkpkg -I .. \
		 ../netclient.cma test_bad_request.ml

clean:
	rm -f *.cmo *.cmi run test_client test_server *.out \
		test_bad_request refuser log/* server.pid server.port

distclean: clean
