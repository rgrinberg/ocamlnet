# Note: [out of date]

# 2nd note: "test_encoding.cgi" is a CGI script; you must invoke
# it through browser and WWW server.

test: test_recode fort
	OCAMLPATH=../.. ocaml test_netencoding.ml
	OCAMLPATH=../.. ocaml test_mimestring.ml
	OCAMLPATH=../.. ocaml test_nethtml.ml
	OCAMLPATH=../.. ocaml test_neturl.ml
	CAML_LD_LIBRARY_PATH=../../netsys ./test_recode

test_recode: test_recode.ml
	OCAMLPATH=../.. \
	ocamlfind ocamlc -o test_recode -package netstring,threads -linkpkg \
		-thread test_recode.ml

speed_conv: speed_conv.ml
	ocamlfind ocamlopt -o speed_conv -package unix,str,pcre -linkpkg \
		../netstring.cmxa ../netmappings_iso.cmx \
		-I .. speed_conv.ml

speed_conv_p: speed_conv.ml
	ocamlfind ocamlopt -p -o speed_conv_p -package unix,str,pcre -linkpkg \
		../netstring.cmxa ../netmappings_iso.cmx \
		-I .. speed_conv.ml

.PHONY: fort
fort:
	cd fort; ./configure; make

distclean: clean
	rm -f *~ test_recode speed_conv speed_conv_p

clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa
	cd fort; make clean

CLEAN:

