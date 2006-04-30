TOP_DIR = ../..
include $(TOP_DIR)/Makefile.rules
include $(TOP_DIR)/Makefile.xrules

XDRFILES = 

RPCGEN_AUX_OPTIONS    = 
RPCGEN_CLIENT_OPTIONS = 
RPCGEN_SERVER_OPTIONS = 

all:
#	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code all

opt:
#	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code opt

#generate: $(XDRFILES:.x=.astamp) $(XDRFILES:.x=.cstamp) 
# Don't generate server stubs

depend: *.ml *.mli
	$(OCAMLDEP) *.ml *.mli >depend

clean:
	rm -f $(CLEAN_LIST)

CLEAN: clean

distclean: clean
	rm -f META

install:
	files=`$(COLLECT_FILES) *.mli *.cmi *.cma *.cmxa *.a *.x dll* META` && \
	$(OCAMLFIND) install rpc-auth-local $$files

uninstall:
	$(OCAMLFIND) remove rpc-auth-local



