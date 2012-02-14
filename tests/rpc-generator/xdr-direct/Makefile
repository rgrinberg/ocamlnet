.PHONY: all build test clean

all: build
	$(MAKE) test

build:
	cd ../xdr && $(MAKE) build RPCGEN_EXTRA=-direct

test:
	cd ../xdr && $(MAKE) test

clean:
	cd ../xdr && $(MAKE) clean

