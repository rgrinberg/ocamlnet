TOP_DIR=../..

PKGNAME  = rpc-generator
ARCHIVE  = rpc_generator
OBJECTS  = rtypes.cmo syntax.cmo options.cmo parser.cmo lexer.cmo rename.cmo \
           generate.cmo
DOBJECTS = 

ALL_EXTRA = ocamlrpcgen$(EXEC_SUFFIX) rpcgen-packlist

PACKLIST = rpcgen-packlist

# rtypes has some conditional stuff:
RTYPES_DEFS = -ppopt -DWORDSIZE_$(WORD_SIZE)
OCAMLC_OPTIONS_FOR_rtypes.ml = \
	-syntax camlp4o -package camlp4.macro $(RTYPES_DEFS)
OCAMLOPT_OPTIONS_FOR_rtypes.ml = \
	-syntax camlp4o -package camlp4.macro $(RTYPES_DEFS)

OCAMLDEP_OPTIONS = -syntax camlp4o -package camlp4.macro

include $(TOP_DIR)/Makefile.rules

ocamlrpcgen$(EXEC_SUFFIX): $(ARCHIVE).cma main.cmo
	rm -f ocamlrpcgen ocamlrpcgen$(EXEC_SUFFIX)
	$(OCAMLC) -custom -o ocamlrpcgen -package unix \
		$(ARCHIVE).cma main.cmo -linkpkg

rpcgen-packlist:
	echo $(BINDIR)/ocamlrpcgen >rpcgen-packlist

include depend
