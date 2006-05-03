# Note: [out of date]

# 2nd note: "test_encoding.cgi" is a CGI script; you must invoke
# it through browser and WWW server.

test: test_recode
	ocaml test_netencoding.ml
	ocaml test_mimestring.ml
	#ocaml test_cgi.ml
	ocaml test_neturl.ml
	LD_LIBRARY_PATH=.. ./test_recode

test_recode: test_recode.ml
	ocamlfind ocamlc -o test_recode -package unix,str,pcre,threads -linkpkg  -I .. \
		../netstring.cma ../netmappings_iso.cmo ../netmappings_jp.cmo \
		../netaccel.cma ../netaccel_link.cmo -thread test_recode.ml

speed_conv: speed_conv.ml
	ocamlfind ocamlopt -o speed_conv -package unix,str,pcre -linkpkg \
		../netstring.cmxa ../netmappings_iso.cmx \
		-I .. speed_conv.ml

speed_conv_p: speed_conv.ml
	ocamlfind ocamlopt -p -o speed_conv_p -package unix,str,pcre -linkpkg \
		../netstring.cmxa ../netmappings_iso.cmx \
		-I .. speed_conv.ml

distclean: clean
	rm -f *~ test_recode speed_conv speed_conv_p

clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa

CLEAN:

