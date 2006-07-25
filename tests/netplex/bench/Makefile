TOP_DIR = ../../..
include $(TOP_DIR)/Makefile.rules
include $(TOP_DIR)/Makefile.xrules

XDRFILES = proto.x

RPCGEN_AUX_OPTIONS    = -int unboxed -hyper int64
RPCGEN_CLIENT_OPTIONS = -int unboxed -hyper int64
RPCGEN_SERVER_OPTIONS = -int unboxed -hyper int64

CLEAN_LIST += socket protosettings protoclient protoserver \
	      *.astamp *.cstamp *.s2stamp \
	      proto_aux.ml proto_aux.mli \
	      proto_clnt.ml proto_clnt.mli \
	      proto_srv.ml proto_srv.mli \
	      proto.conf.1 depend proto.log proto.pid \
	      socket1 socket2

clean::
	rm -rf sockdir

all:
	$(MAKE) gen
	$(MAKE) depend
	$(MAKE) -f Makefile.code all

opt:
	$(MAKE) gen
	$(MAKE) depend
	$(MAKE) -f Makefile.code opt

test: all
	./prototest all

gen: $(XDRFILES:.x=.astamp) $(XDRFILES:.x=.cstamp) $(XDRFILES:.x=.s2stamp)
