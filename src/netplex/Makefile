TOP_DIR=../..

PKGNAME  = netplex
OBJECTS  = netplex_mp.cmo netplex_log.cmo netplex_controller.cmo \
	   netplex_container.cmo
DOBJECTS = netplex_types.mli netplex_mp.mli netplex_log.mli \
	   netplex_controller.mli netplex_container.mli

INCLUDES += -I ../equeue -I ../rpc -I ../shell -I ../netstring
# Note: Instead of shell use unix-exts

include $(TOP_DIR)/Makefile.rules

include depend
