TOP_DIR=..

PKGNAME  = cgi
OBJECTS  = netcgi_env.cmo netcgi_types.cmo netcgi.cmo \
	   netcgi_jserv.cmo netcgi_jserv_ajp12.cmo \
	   netcgi_jserv_app.cmo netcgi_fcgi_10.cmo \
	   netcgi_fcgi.cmo

OCAMLDOC = netcgi_env.mli netcgi_types.mli netcgi.mli \
	   netcgi_jserv.mli netcgi_jserv_ajp12.mli netcgi_jserv_app.mli \
	   netcgi_fcgi_10.mli netcgi_fcgi.mli

HTMLDIR?=html

include $(TOP_DIR)/Makefile.rules

INCLUDES += -I ../netstring

INSTALL_EXTRA += LICENSE

ocamldoc.dump:
	ocamldoc `ocamlfind query -i-format pcre` -I ../netstring -v -stars -dump ocamldoc.dump $(OCAMLDOC)

clean::
	rm -f ocamldoc.dump

include depend
