TOP_DIR=../..

PKGNAME = netclient
OBJECTS =  http_client.cmo telnet_client.cmo \
	   ftp_data_endpoint.cmo ftp_client.cmo
DOBJECTS = http_client.mli telnet_client.mli \
	   ftp_data_endpoint.mli ftp_client.mli

ALLMT_EXTRA = http_client_mt.cmo
OPTMT_EXTRA = http_client_mt.cmx

OCAMLC_OPTIONS_FOR_http_client_mt.ml = $(THREAD)
OCAMLOPT_OPTIONS_FOR_http_client_mt.ml = $(THREAD)

INCLUDES = $(INC_EQUEUE) $(INC_NETSTRING) $(INC_NETSYS)

include $(TOP_DIR)/Makefile.rules

include depend
