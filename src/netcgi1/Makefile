TOP_DIR=../..

PKGNAME  = cgi
OBJECTS  = netcgi_env.cmo netcgi_types.cmo netcgi.cmo \
	   netcgi_jserv.cmo netcgi_jserv_ajp12.cmo \
	   netcgi_jserv_app.cmo netcgi_fcgi_10.cmo \
	   netcgi_fcgi.cmo netcgi1_compat.cmo

DOBJECTS = netcgi_env.mli netcgi_types.mli netcgi.mli \
	   netcgi_jserv.mli netcgi_jserv_ajp12.mli netcgi_jserv_app.mli \
	   netcgi_fcgi_10.mli netcgi_fcgi.mli netcgi1_compat.mli

INCLUDES += $(INC_NETSTRING) $(INC_NETSYS)

include $(TOP_DIR)/Makefile.rules

include depend
