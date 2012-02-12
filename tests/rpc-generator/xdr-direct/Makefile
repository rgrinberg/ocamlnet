.PHONY: all build test

all: build
	$(MAKE) test

build:
	cd ../xdr && $(MAKE) build RPCGEN_EXTRA=-direct

test:
	cd ../xdr && $(MAKE) test


