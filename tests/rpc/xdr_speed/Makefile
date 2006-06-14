TOP_DIR = ../..
include $(TOP_DIR)/Makefile.rules
include $(TOP_DIR)/Makefile.xrules

XDRFILES = proto.x

RPCGEN_AUX_OPTIONS    = -int int32 -hyper int64
RPCGEN_CLIENT_OPTIONS = -int int32 -hyper int64
RPCGEN_SERVER_OPTIONS = -int int32 -hyper int64

OCAMLRPCGEN = ../../src/generator/ocamlrpcgen

all:
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code all

opt:
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code opt

test: all
	./prototest all

generate: $(XDRFILES:.x=.astamp) $(XDRFILES:.x=.cstamp) $(XDRFILES:.x=.sstamp)

depend: *.ml *.mli
	$(OCAMLDEP) *.ml *.mli >depend

clean:
	rm -f $(CLEAN_LIST) protospeed_byte protospeed_opt

CLEAN: clean

distclean: clean
#	rm -f META
