TOP_DIR = ../..
include $(TOP_DIR)/Makefile.rules

all:
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code all

opt:
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code opt

generate: rpc_portmapper.astamp rpc_portmapper.cstamp

rpc_portmapper.astamp: rpc_portmapper.x
	../generator/ocamlrpcgen -aux rpc_portmapper.x

rpc_portmapper.cstamp: rpc_portmapper.x
	../generator/ocamlrpcgen -clnt rpc_portmapper.x

depend:
	$(OCAMLDEP) *.ml *.mli >depend

clean:
	rm -f $(CLEAN_LIST)

CLEAN: clean

distclean: clean
	rm -f META

# We install rtypes.cmx and xdr.cmx to allow inlining

install:
	files=`$(COLLECT_FILES) *.mli *.cmi *.cma *.cmxa *.a rtypes.cmx xdr.cmx META` && \
	$(OCAMLFIND) install rpc $$files

uninstall:
	$(OCAMLFIND) remove rpc
