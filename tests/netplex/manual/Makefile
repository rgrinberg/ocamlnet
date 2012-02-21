TOP_DIR = ../../..

all: http_daemon

include $(TOP_DIR)/Makefile.rules

http_daemon: http_daemon.ml
	OCAMLPATH=$(TOP_DIR)/src \
		ocamlfind ocamlc -o http_daemon \
			-package netplex,nethttpd -linkpkg \
			http_daemon.ml

http_daemon.opt: http_daemon.ml
	OCAMLPATH=$(TOP_DIR)/src \
		ocamlfind ocamlopt -o http_daemon.opt \
			-package netplex,nethttpd -linkpkg \
			http_daemon.ml

# -------------------------------------------------------------------

speed:
	$(MAKE) speed_gen
	OCAMLPATH=$(TOP_DIR)/src \
	    $(MAKE) speed_server speed_client REQUIRES=netplex

SPEED = speed_proto_aux.cmx speed_proto_srv.cmx speed_proto_clnt.cmx

speed_gen: speed_proto_aux.ml speed_proto_clnt.ml speed_proto_srv.ml

speed_proto_aux.ml: speed_proto.x
	$(TOP_DIR)/src/rpc-generator/ocamlrpcgen -aux speed_proto.x

speed_proto_aux.cmx: speed_proto_aux.cmi

speed_proto_clnt.ml: speed_proto.x
	$(TOP_DIR)/src/rpc-generator/ocamlrpcgen -clnt speed_proto.x

speed_proto_clnt.cmx: speed_proto_clnt.cmi

speed_proto_srv.ml: speed_proto.x
	$(TOP_DIR)/src/rpc-generator/ocamlrpcgen -srv2 speed_proto.x

speed_proto_srv.cmx: speed_proto_srv.cmi

speed_server: $(SPEED) speed_server.cmx
	OCAMLPATH=$(TOP_DIR)/src \
	    ocamlfind ocamlopt -o speed_server \
	        $(OCAMLOPT_OPTIONS) \
		-package netplex -linkpkg $(SPEED) speed_server.cmx

speed_client: $(SPEED) speed_client.cmx
	OCAMLPATH=$(TOP_DIR)/src \
	    ocamlfind ocamlopt -o speed_client \
	        $(OCAMLOPT_OPTIONS) \
		-package netplex -linkpkg $(SPEED) speed_client.cmx

clean::
	rm -f *.cmi *.cmo *.cma *.cmxa *.cmx *.o *.a
	rm -f http_daemon speed_server speed_client
