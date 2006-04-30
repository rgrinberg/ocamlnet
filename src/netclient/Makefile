# make all: 		make bytecode archive
# make opt: 		make native archive
# make install: 	install bytecode archive, and if present, native archive
# make uninstall: 	uninstall package
# make clean: 		remove intermediate files
# make distclean: 	remove any superflous files
# make release: 	cleanup, create archive, tag CVS module 
#			(for developers)

#----------------------------------------------------------------------
# specific rules for this package:

OBJECTS  = http_client.cmo telnet_client.cmo ftp_data_endpoint.cmo ftp_client.cmo
#ftp_data_endpoint.cmo
XOBJECTS = $(OBJECTS:.cmo=.cmx)
ARCHIVE  = netclient.cma
XARCHIVE = netclient.cmxa
NAME     = netclient
REQUIRES = unix equeue netstring

BYTE_THREAD = -vmthread
NAT_THREAD = -thread

all: $(ARCHIVE) http_client_mt.cmo

opt: $(XARCHIVE) http_client_mt.cmx


$(ARCHIVE): $(OBJECTS) 
	$(OCAMLC) -a -o $(ARCHIVE) $(OBJECTS)

$(XARCHIVE): $(XOBJECTS) 
	$(OCAMLOPT) -a -o $(XARCHIVE) $(XOBJECTS)

http_client_mt.cmo: http_client_mt.ml
	$(OCAMLC) -vmthread -c http_client_mt.ml

# If ocamlopt does not support -thread, this will fail. Just ignore.
http_client_mt.cmx: http_client_mt.ml
	-$(OCAMLOPT) -thread -c http_client_mt.ml

#----------------------------------------------------------------------
# general rules:

OPTIONS   =
OCAMLC    = ocamlfind ocamlc  -g $(OPTIONS) -package "$(REQUIRES)"
OCAMLOPT  = ocamlfind ocamlopt $(OPTIONS)   -package "$(REQUIRES)"
OCAMLDEP  = ocamldep $(OPTIONS)
OCAMLFIND = ocamlfind
OCAMLDOC  = ocamlfind doc -package "$(REQUIRES)"

depend: *.ml *.mli
	$(OCAMLDEP) *.ml *.mli >depend
#	$(OCAMLDEP) http_client.ml http_client.mli \
#		telnet_client.ml telnet_client.mli \
#		http_client_mt.mli http_client_mt.ml >depend 

.PHONY: install
install: all
	{ test ! -f $(XARCHIVE) || extra1="*.cmxa *.a"; }; \
	{ test ! -f http_client_mt.cmx || extra2="http_client_mt.cmx http_client_mt.o"; }; \
	$(OCAMLFIND) install $(NAME) *.mli *.cmi *.cma http_client_mt.cmo META $$extra1 $$extra2

.PHONY: uninstall
uninstall:
	$(OCAMLFIND) remove $(NAME)

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa
	[ ! -d tests ] || $(MAKE) -C tests clean
	rm -rf doc

.PHONY: distclean
distclean: clean
	rm -f *~ depend depend.pkg
	$(MAKE) -C tests distclean
	$(MAKE) -C tests_std distclean
	$(MAKE) -C examples/spider distclean
	$(MAKE) -C examples/telnet_labltk distclean

RELEASE: META
	awk '/version/ { print substr($$3,2,length($$3)-2) }' META >RELEASE

.PHONY: dist
dist: RELEASE
	r=`head -1 RELEASE`; cd ..; gtar czf $(NAME)-$$r.tar.gz --exclude='*/CVS*' --exclude="*/depend.pkg" --exclude="*/depend" $(NAME)

.PHONY: tag-release
tag-release: RELEASE
	r=`head -1 RELEASE | sed -e s/\\\./-/g`; cd ..; cvs tag -F $(NAME)-$$r $(NAME)

.PHONY: release
release: distclean
	$(MAKE) tag-release
	$(MAKE) dist

.PHONY: doc
doc: 
	rm -rf doc
	mkdir -p doc
	$(OCAMLDOC) -stars -html -d doc -t "The Netclient Manual" *.mli

.ml.cmx:
	$(OCAMLOPT) -c $<

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.SUFFIXES: .cmo .cmi .cmx .ml .mli

include depend
