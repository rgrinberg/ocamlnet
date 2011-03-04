TOP_DIR=../..

PKGNAME  = rpc-generator
ARCHIVE  = rpc_generator
OBJECTS  = syntax.cmo options.cmo parser.cmo lexer.cmo rename.cmo \
           generate.cmo
DOBJECTS = 

ALL_EXTRA = ocamlrpcgen$(EXEC_SUFFIX) rpcgen-packlist

PACKLIST = rpcgen-packlist

#OCAMLDEP_OPTIONS = -syntax camlp4o -package camlp4.macro

INCLUDES += $(INC_NETSTRING)

include $(TOP_DIR)/Makefile.rules

ocamlrpcgen$(EXEC_SUFFIX): $(ARCHIVE).cma main.cmo
	rm -f ocamlrpcgen ocamlrpcgen$(EXEC_SUFFIX)
	OCAMLPATH=.. \
	$(OCAMLC) -custom -o ocamlrpcgen -package unix,netstring \
		$(ARCHIVE).cma main.cmo -linkpkg

rpcgen-packlist:
	echo $(BINDIR)/ocamlrpcgen >rpcgen-packlist

include depend
