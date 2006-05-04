TOP_DIR=../..

PKGNAME  = nethttpd
OBJECTS  = nethttpd_types.cmo nethttpd_kernel.cmo nethttpd_reactor.cmo \
	  nethttpd_engine.cmo nethttpd_services.cmo

OCAMLDOC = nethttpd_types.mli nethttpd_kernel.mli nethttpd_reactor.mli \
	  nethttpd_engine.mli nethttpd_services.mli

INCLUDES += -I ../equeue -I ../netstring -I ../cgi
PP_OPTIONS = -package "camlp4, camlp4.macro" -syntax "camlp4o"

#PP_OPTIONS_FINDLIB += -ppopt "-DTesting"
# do this to enable test coverage calculations

INSTALL_EXTRA += LICENSE

include $(TOP_DIR)/Makefile.rules

include depend
