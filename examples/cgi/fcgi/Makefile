.PHONY: all
all: add_fastcgi.fcgi add_fastcgi.opt

.SUFFIXES: .ml .fcgi .opt

.ml.opt:
	ocamlfind ocamlopt -o $@ -package cgi -linkpkg $<
	ln -s $@ $@.fcgi

.ml.fcgi:
	ocamlfind ocamlc -g -o $@ -package cgi -linkpkg $<

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.fcgi *.opt *.cmx *.o
