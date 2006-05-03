TOP_DIR=..

PKGNAME  = nethttpd
OBJECTS  = nethttpd_types.cmo nethttpd_kernel.cmo nethttpd_reactor.cmo \
	  nethttpd_engine.cmo nethttpd_services.cmo

OCAMLDOC = nethttpd_types.mli nethttpd_kernel.mli nethttpd_reactor.mli \
	  nethttpd_engine.mli nethttpd_services.mli


HTMLDIR?=html

include $(TOP_DIR)/Makefile.rules

INCLUDES += -I ../netstring -I ../cgi
PP_OPTIONS_FINDLIB = -package "camlp4, camlp4.macro" -syntax "camlp4o"
PP_OPTIONS_NON_FINDLIB = -pp "camlp4o pa_macro.cmo"

#PP_OPTIONS_FINDLIB += -ppopt "-DTesting"
# do this to enable test coverage calculations

INSTALL_EXTRA += LICENSE

ocamldoc.dump:
	ocamldoc `ocamlfind query -i-format equeue pcre` -I ../netstring -I ../cgi -v -stars -dump ocamldoc.dump $(OCAMLDOC)

clean::
	rm -f ocamldoc.dump

include depend
