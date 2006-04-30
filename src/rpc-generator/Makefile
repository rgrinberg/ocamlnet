TOP_DIR = ../..
include $(TOP_DIR)/Makefile.rules

all:
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code all

opt:
	true

depend:
	$(OCAMLDEP) *.ml *.mli >depend

generate: parser.ml lexer.ml rtypes.ml rtypes.mli

lexer.ml: parser.ml

rtypes.ml:
	cp ../rpc/rtypes.ml .

rtypes.mli:
	cp ../rpc/rtypes.mli .

clean:
	rm -f $(CLEAN_LIST) META ocamlrpcgen parser.ml parser.mli lexer.ml

CLEAN: clean
	$(MAKE) -C tests CLEAN

distclean: clean
	$(MAKE) -C tests distclean

install:
	test -n "$(BINDIR)"
	cp ocamlrpcgen $(BINDIR)

uninstall:
	test -n "$(BINDIR)"
	rm -f $(BINDIR)/ocamlrpcgen
