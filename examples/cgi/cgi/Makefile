.PHONY: all
all: add.cgi counter.cgi filemanager.cgi mailbox.cgi

.SUFFIXES: .ml .cgi .opt

.ml.opt:
	ocamlfind ocamlopt -o $@ -package cgi -linkpkg $<
	ln -s $@ $@.cgi

.ml.cgi:
	ocamlfind ocamlc -g -o $@ -package cgi -linkpkg $<

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cgi *.opt *.cmx *.o
