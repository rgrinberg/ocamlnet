TOP_DIR=../..

PKGNAME  = netplex
OBJECTS  = netplex_mp.cmo netplex_log.cmo \
	   netplex_ctrl_aux.cmo netplex_ctrl_clnt.cmo netplex_ctrl_srv.cmo \
	   netplex_container.cmo netplex_sockserv.cmo netplex_workload.cmo \
	   netplex_controller.cmo
DOBJECTS = netplex_types.mli netplex_mp.mli netplex_log.mli \
	   netplex_controller.mli netplex_container.mli \
	   netplex_sockserv.mli netplex_workload.mli

INCLUDES += -I ../equeue -I ../rpc -I ../shell -I ../netstring
# Note: Instead of shell use unix-exts

ALL_EXTRA = netplex-admin

include $(TOP_DIR)/Makefile.rules

netplex-admin: netplex.cma netplex_admin.cmo
	$(OCAMLC) -o netplex-admin -custom \
		unix.cma equeue.cma netstring.cma rpc.cma shell.cma \
		netplex.cma netplex_admin.cmo

CLEAN_LIST += netplex-admin

include depend
