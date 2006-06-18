TOP_DIR=../..

PKGNAME  = rpc-generator
ARCHIVE  = rpc_generator
OBJECTS  = rtypes.cmo syntax.cmo options.cmo parser.cmo lexer.cmo rename.cmo \
           generate.cmo
DOBJECTS = 

ALL_EXTRA = ocamlrpcgen$(EXEC_SUFFIX)
INSTALL_EXTRA = rpcgen-packlist

INSTOTHER = install-rpcgen
PACKLIST = rpcgen-packlist

include $(TOP_DIR)/Makefile.rules

ocamlrpcgen$(EXEC_SUFFIX): $(ARCHIVE).cma main.cmo
	$(OCAMLC) -custom -o ocamlrpcgen -package unix \
		$(ARCHIVE).cma main.cmo -linkpkg

.PHONY: install-rpcgen
install-rpcgen:
	mkdir -p $(DESTDIR)$(BINDIR)
	cp ocamlrpcgen$(EXEC_SUFFIX) $(DESTDIR)$(BINDIR)
	@echo "Installed ocamlrpcgen$(EXEC_SUFFIX) into $(DESTDIR)$(BINDIR)"

rpcgen-packlist:
	echo $(BINDIR)/ocamlrpcgen >rpcgen-packlist

include depend
