TOP_DIR=../..

PKGNAME  = netcgi
OBJECTS  = netcgi_common.cmo netcgi.cmo \
	netcgi_ajp.cmo netcgi_scgi.cmo netcgi_cgi.cmo \
	netcgi_fcgi.cmo  netcgi_dbi.cmo netcgi_modtpl.cmo
#	netcgi_test.cmo netcgi_template.cmo

DOBJECTS = netcgi_common.mli netcgi.mli \
	netcgi_ajp.mli netcgi_scgi.mli netcgi_cgi.mli \
	netcgi_fcgi.mli  netcgi_dbi.mli netcgi_modtpl.mli \
	netcgi_porting.txt
#	netcgi_test.mli

INCLUDES += $(INC_NETSTRING)

# We need to know whether to enable the Apache mod connector:
-include $(TOP_DIR)/Makefile.conf

# For the Apache mod connector, if enabled
ifdef APACHE_MAJOR
OBJECTS += netcgi_mod.cmo
DOBJECTS += netcgi_mod.mli

INCLUDES += -I mod_caml/

BUILD_EXTRA=mod_caml

# FIXME: mod_caml requires a special installation procedure

endif

netcgi_mod.cmi: netcgi_mod.mli mod_caml/apache.cmi \
	mod_caml/apache.cmi mod_caml/mod_caml.cmi

netcgi_mod.cmo: netcgi_mod.cmi mod_caml/apache.cmo \
	mod_caml/mod_caml_config.cmo mod_caml/apache.cmo mod_caml/mod_caml.cmo

mod_caml/%:
	cd mod_caml/; \
	$(MAKE) PACKAGE=$(PKGNAME) VERSION=$(VERSION) \
		-f Makefile.pre realgenerate; \
	$(MAKE) $*


include $(TOP_DIR)/Makefile.rules

include depend

clean::
	cd mod_caml/; $(MAKE) clean