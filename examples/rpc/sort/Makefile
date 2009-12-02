TOP_DIR = ../../..
include $(TOP_DIR)/Makefile.rules
include $(TOP_DIR)/Makefile.xrules

XDRFILES = sort_proto.x

RPCGEN_AUX_OPTIONS    = -int unboxed -hyper int64
RPCGEN_CLIENT_OPTIONS = -int unboxed -hyper int64
RPCGEN_SERVER_OPTIONS = -int unboxed -hyper int64

CLEAN_LIST += *.astamp *.cstamp *.s2stamp *.run *.opt \
              sort_proto_aux.ml sort_proto_aux.mli \
	      sort_proto_clnt.ml sort_proto_clnt.mli \
	      sort_proto_srv.ml sort_proto_srv.mli

all:
	$(MAKE) gen
	$(MAKE) depend
	$(MAKE) -f Makefile.code all

opt:
	$(MAKE) gen
	$(MAKE) depend
	$(MAKE) -f Makefile.code opt

gen: $(XDRFILES:.x=.astamp) $(XDRFILES:.x=.cstamp) $(XDRFILES:.x=.s2stamp)
