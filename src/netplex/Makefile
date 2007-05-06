TOP_DIR=../..

PKGNAME  = netplex
OBJECTS  = netplex_cenv.cmo netplex_mp.cmo netplex_log.cmo \
	   netplex_ctrl_aux.cmo netplex_ctrl_clnt.cmo netplex_ctrl_srv.cmo \
	   netplex_container.cmo netplex_sockserv.cmo netplex_workload.cmo \
	   netplex_kit.cmo netplex_controller.cmo \
           netplex_config.cmo netplex_main.cmo rpc_netplex.cmo
DOBJECTS = netplex_types.mli netplex_mp.mli netplex_mt.mli netplex_log.mli \
	   netplex_controller.mli netplex_container.mli \
	   netplex_sockserv.mli netplex_workload.mli netplex_main.mli \
	   netplex_config.mli netplex_kit.mli rpc_netplex.mli \
	   netplex_cenv.mli \
	   netplex_intro.txt

INCLUDES += $(INC_EQUEUE) $(INC_RPC) $(INC_NETSTRING) $(INC_NETSYS)

ALL_EXTRA = netplex-admin$(EXEC_SUFFIX) netplex-packlist 
ALLMT_EXTRA = netplex_mt.cmo
OPTMT_EXTRA = netplex_mt.cmx

PP_OPTIONS = -syntax camlp4o -package camlp4

OCAMLC_OPTIONS_FOR_netplex_mt.ml = $(THREAD)
OCAMLOPT_OPTIONS_FOR_netplex_mt.ml = $(THREAD)

PACKLIST = netplex-packlist

include $(TOP_DIR)/Makefile.rules

netplex-admin$(EXEC_SUFFIX): netplex.cma netplex_admin.cmo
	$(OCAMLC) -o netplex-admin -custom \
		unix.cma netsys.cma equeue.cma netstring.cma rpc.cma \
		netplex.cma netplex_admin.cmo

netplex-packlist:
	echo $(BINDIR)/netplex-admin >netplex-packlist

include depend
