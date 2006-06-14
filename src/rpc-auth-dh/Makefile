TOP_DIR=../..

PKGNAME  = rpc-auth-dh
ARCHIVE  = rpc_auth_dh
OBJECTS  = rpc_time.cmo \
           rpc_key_aux.cmo rpc_key_clnt.cmo rpc_key_service.cmo rpc_auth_dh.cmo
DOBJECTS = 

include $(TOP_DIR)/Makefile.rules

INCLUDES += -I ../rpc -I ../equeue
REQUIRES += cryptgps

include depend
