TOP_DIR = ../../..
include $(TOP_DIR)/Makefile.rules
include $(TOP_DIR)/Makefile.xrules

XDRFILES = adder.x

RPCGEN_AUX_OPTIONS    = -int unboxed
RPCGEN_CLIENT_OPTIONS = -int unboxed
RPCGEN_SERVER_OPTIONS = -int unboxed

all: adder.x
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code all

opt: adder.x
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code opt

adder.x: ../adder.x
	cp ../adder.x .

generate: $(XDRFILES:.x=.astamp) $(XDRFILES:.x=.cstamp) $(XDRFILES:.x=.sstamp)

depend: *.ml *.mli
	$(OCAMLDEP) *.ml *.mli >depend

clean:
	rm -f $(CLEAN_LIST)

CLEAN: clean

distclean: clean
	rm -f META

#install:
#	files=`$(COLLECT_FILES) *.mli *.cmi *.cma *.cmxa *.a META` && \
#	$(OCAMLFIND) install rpc $$files

#uninstall:
#	$(OCAMLFIND) remove rpc
