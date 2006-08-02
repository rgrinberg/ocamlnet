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

include $(TOP_DIR)/Makefile.rules

include depend
