TOP_DIR=..

PKGNAME  = rpc-generator
ARCHIVE  = rpc_generator
OBJECTS  = rtypes.cmo syntax.cmo options.cmo parser.cmo lexer.cmo rename.cmo \
           generate.cmo
OCAMLDOC = 

ALL_EXTRA = ocamlrpcgen

include $(TOP_DIR)/Makefile.rules

ocamlrpcgen: $(ARCHIVE).cma main.cmo
	$(OCAMLC) -custom -o ocamlrpcgen $(ARCHIVE).cma main.cmo -linkpkg

ocamldoc.dump: $(OCAMLDOC)
        ocamldoc -v -stars -dump ocamldoc.dump $(OCAMLDOC)

include depend
