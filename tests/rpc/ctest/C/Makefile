SERVER_OBJ = adder_server.o adder_svc.o adder_xdr.o
CLIENT_OBJ = adder_client.o adder_clnt.o adder_xdr.o

all: adder_server adder_client

adder_client: $(CLIENT_OBJ)
	$(CC) -g -o adder_client $(CLIENT_OBJ)

adder_server: $(SERVER_OBJ)
	$(CC) -o adder_server $(SERVER_OBJ)

adder.h: adder.x adder_main.c
	rm -f adder_xdr.c adder.h adder_svc.c
	rpcgen -b -N -c -o adder_xdr.c adder.x
	rpcgen -b -N -h -o adder.h adder.x
	rpcgen -b -N -m -o adder_svc.c adder.x
	rpcgen -b -N -l -o adder_clnt.c adder.x
	cat adder_main.c >>adder_svc.c

adder.h: adder.x
adder_xdr.c: adder.x
adder_svc.c: adder.x
adder_clnt.c: adder.x

adder_server.c: adder.h
adder_client.c: adder.h

adder.x: ../adder.x
	cp ../adder.x .

clean:
	rm -f *.o *_svc.c *_xdr.c *_clnt.c 
	rm -f adder.h adder_server adder_client
