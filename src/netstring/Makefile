TOP_DIR=..

OBJECTS  = netstring_pcre.cmo netstring_str.cmo netbuffer.cmo \
	   netaux.cmo netchannels.cmo netdate.cmo \
	   netdb.cmo netmappings.cmo netconversion.cmo netulex.cmo \
           netencoding.cmo netstream.cmo \
	   mimestring.cmo \
           nethtml_scanner.cmo nethtml.cmo \
	   neturl.cmo \
	   netaddress.cmo \
	   netmime.cmo netsendmail.cmo nethttp.cmo \

PKGNAME  = netstring

ISO_MAPPINGS   = mappings/iso*.unimap
JP_MAPPINGS    = mappings/jis*.*map
OTHER_MAPPINGS = mappings/cp*.unimap \
                 mappings/adobe*.unimap \
                 mappings/koi*.unimap \
                 mappings/mac*.unimap \
                 mappings/windows*.unimap

# netconversion.cmx: Allow cross-library inlining for this module

INSTALL_EXTRA = netstring_mt.cmo      netstring_mt.cmx      netstring_mt.o     \
	        netmappings_iso.cmo   netmappings_iso.cmx   netmappings_iso.o  \
	        netmappings_jp.cmo   netmappings_jp.cmx   netmappings_jp.o  \
	        netmappings_other.cmo netmappings_other.cmx netmappings_other.o\
		netmappings_min.cmo  netmappings_min.cmx netmappings_min.o \
		netconversion.cmx \
		netstring_top.cmo \
		compatcgi.cma compatcgi.cmxa compatcgi.a \
	 	cgi_mt.cmo cgi_mt.cmx cgi_mt.o \
		netdb-packlist \
		dllnetaccel_c.* netaccel_link.cmo \
		LICENSE

OCAMLDOC = netconversion.mli netchannels.mli netstream.mli mimestring.mli \
	netmime.mli netsendmail.mli neturl.mli netaddress.mli netbuffer.mli \
	netdate.mli netencoding.mli netulex.mli netaccel.mli \
	netaccel_link.mli nethtml.mli netstring_str.mli netstring_pcre.mli \
	netstring_mt.mli \
	netmappings.mli netaux.mli nethttp.mli

PP_OPTIONS = -package camlp4 -syntax camlp4o

INSTOTHER = install-netdb
UNINSTOTHER = uninstall-netdb

OBJ_MT_BYTE = $(NETSTRING_PARTS:=.byte.mt)
OBJ_MT_NAT  = $(NETSTRING_PARTS:=.opt.mt)

OBJ_MT_BYTE_SWITCH = $(OBJ_MT_BYTE)
OBJ_MT_NAT_SWITCH = $(OBJ_MT_NAT)
# May be overriden later (Makefile.conf)

.PHONY: all opt 
.PHONY: core.byte core.opt compatcgi.byte compatcgi.opt
.PHONY: core.byte.mt core.opt.mt compatcgi.byte.mt compatcgi.opt.mt

include $(TOP_DIR)/Makefile.rules

# NETSTRING_PARTS: list of words:
# - core (always): The core parts of netstring
# - compatcgi (optional): The legacy Cgi module

all: $(NETSTRING_PARTS:=.byte) $(OBJ_MT_BYTE_SWITCH) 

opt: $(NETSTRING_PARTS:=.opt)  $(OBJ_MT_NAT_SWITCH)

core.byte: $(ARCHIVE) netdb-packlist \
     netstring_top.cmo \
     netmappings_iso.cmo netmappings_other.cmo netmappings_jp.cmo \
     netmappings_min.cmo netaccel_c.a

core.opt: $(XARCHIVE) netdb-packlist \
     netmappings_iso.cmx netmappings_other.cmx netmappings_jp.cmx \
     netmappings_min.cmx

core.byte.mt: netstring_mt.cmo

core.opt.mt: netstring_mt.cmx

compatcgi.byte: compatcgi.cma

compatcgi.opt: compatcgi.cmxa

compatcgi.byte.mt: cgi_mt.cmo

compatcgi.opt.mt: cgi_mt.cmx

compatcgi.cma: core.byte cgi.cmo
	$(OCAMLC) -a -o compatcgi.cma cgi.cmo

compatcgi.cmxa: core.opt cgi.cmx
	$(OCAMLOPT) -a -o compatcgi.cmxa cgi.cmx

netdb.ml: netdb.mlp netdb.cmi
	sed -e 's|@NET_DB_DIR@|$(NET_DB_DIR)|' netdb.mlp >netdb.ml

.PHONY: mk_netdb
mk_netdb: tools_done netmappings_iso.pmap netmappings_other.pmap netmappings_jp.pmap
	mkdir -p netdb
	cd netdb && \
		../tools/unimap_to_ocaml/unimap_to_ocaml -netdb ../*.pmap

netmappings_iso.pmap: tools_done
	test ! -d mappings || tools/unimap_to_ocaml/unimap_to_ocaml \
		-o netmappings_iso.pmap -pmap $(ISO_MAPPINGS)

netmappings_iso.ml: netmappings_iso.pmap netdb.cmi
	tools/unimap_to_ocaml/unimap_to_ocaml \
		-o netmappings_iso.ml netmappings_iso.pmap

netmappings_jp.pmap: tools_done
	test ! -d mappings || tools/unimap_to_ocaml/unimap_to_ocaml \
		-o netmappings_jp.pmap -pmap $(JP_MAPPINGS)

#	test ! -f mappings/Unihan.txt.bz2 || \
#	   bzip2 -c -d mappings/Unihan.txt.bz2 | \
# 	   tools/unimap_to_ocaml/unihan_extract \
#	       -o netmappings_jp.pmap -jis0208 -jis0212

netmappings_jp.ml: netmappings_jp.pmap netdb.cmi
	tools/unimap_to_ocaml/unimap_to_ocaml \
		-o netmappings_jp.ml netmappings_jp.pmap

netmappings_other.pmap: tools_done
	test ! -d mappings || tools/unimap_to_ocaml/unimap_to_ocaml \
		-o netmappings_other.pmap -pmap $(OTHER_MAPPINGS)

netmappings_other.ml: netmappings_other.pmap netdb.cmi
	tools/unimap_to_ocaml/unimap_to_ocaml \
		-o netmappings_other.ml netmappings_other.pmap

netaccel_c.a: netaccel_c.o netaccel.cmo netaccel_link.cmo
	$(OCAMLMKLIB) -o netaccel -oc netaccel_c netaccel_c.o netaccel.cmo

tools_done:
	$(MAKE) -C tools
	touch tools_done

netdate.ml: netdate.mlp
	$(CAMLP4) pa_o.cmo pa_op.cmo pr_o.cmo -impl $< > $@

depend:: netdate.ml

ocamldoc.dump: $(OCAMLDOC)
	ocamldoc `ocamlfind query -i-format pcre` -v -stars -dump ocamldoc.dump $(OCAMLDOC)

#----------------------------------------------------------------------
# general rules:

clean::
	rm -f netmappings_iso.ml netmappings_other.ml
	rm -f -r netdb
	test ! -d mappings || rm -f netmappings_iso.pmap netmappings_other.pmap
	rm -f tools_done
	rm -f ocamldoc.dump

distclean::
	$(MAKE) -C tests distclean
	$(MAKE) -C doc distclean
	$(MAKE) -C tools distclean


-include depend

netstring_mt.cmo: netstring_mt.cmi netmappings.cmi netstring_mt.mtml 
netstring_mt.cmx: netstring_mt.cmi netmappings.cmi \
	netmappings.cmx netstring_mt.mtml

cgi_mt.cmo: cgi.cmi
cgi_mt.cmx: cgi.cmi cgi.cmx
