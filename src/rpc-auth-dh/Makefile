TOP_DIR = ../..
include $(TOP_DIR)/Makefile.rules
include $(TOP_DIR)/Makefile.xrules

OCAMLRPCGEN = ../generator/ocamlrpcgen

XDRFILES = rpc_key.x

RPCGEN_AUX_OPTIONS    = -int int32 -hyper int64
RPCGEN_CLIENT_OPTIONS = -int int32 -hyper int64
RPCGEN_SERVER_OPTIONS = -int int32 -hyper int64

all:
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code all

opt:
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code opt

generate: $(XDRFILES:.x=.astamp) $(XDRFILES:.x=.cstamp) 
# Don't generate server stubs

depend: *.ml *.mli
	$(OCAMLDEP) *.ml *.mli >depend

clean:
	rm -f $(CLEAN_LIST)

CLEAN: clean

distclean: clean
	rm -f META

install:
	files=`$(COLLECT_FILES) *.mli *.cmi *.cma *.cmxa *.a *.x META` && \
	$(OCAMLFIND) install rpc-auth-dh $$files

uninstall:
	$(OCAMLFIND) remove rpc-auth-dh



