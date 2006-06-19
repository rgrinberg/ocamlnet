TOP_DIR=../..

PKGNAME  = rpc-generator
ARCHIVE  = rpc_generator
OBJECTS  = rtypes.cmo syntax.cmo options.cmo parser.cmo lexer.cmo rename.cmo \
           generate.cmo
DOBJECTS = 

ALL_EXTRA = ocamlrpcgen$(EXEC_SUFFIX) rpcgen-packlist

PACKLIST = rpcgen-packlist

include $(TOP_DIR)/Makefile.rules

ocamlrpcgen$(EXEC_SUFFIX): $(ARCHIVE).cma main.cmo
	$(OCAMLC) -custom -o ocamlrpcgen -package unix \
		$(ARCHIVE).cma main.cmo -linkpkg

rpcgen-packlist:
	echo $(BINDIR)/ocamlrpcgen >rpcgen-packlist

include depend
