TOP_DIR=../..

PKGNAME = netmech-scram
OBJECTS = netmech_scram.cmo netmech_scram_gssapi.cmo
DOBJECTS = netmech_scram.mli netmech_scram_gssapi.mli

INCLUDES += $(INC_NETSYS) $(INC_NETSTRING) -I ../netgssapi
REQUIRES += cryptokit

include $(TOP_DIR)/Makefile.rules

include depend
