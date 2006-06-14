/*
 * This is sample code generated by rpcgen.
 * These are only templates and you can use them
 * as a guideline for developing your own functions.
 */

#include "adder.h"
#include <rpc/auth.h>
#include <rpc/auth_des.h>


int radd( char* host, int a, int b ) {
	CLIENT *clnt;
	struct sockaddr_in timehost;
	int  *result_1;
	clnt = clnt_create(host, ADDER, V1, "tcp");
	if (clnt == NULL) {
		clnt_pcreateerror(host);
		exit(1);
	}
	timehost.sin_family = AF_INET;
	timehost.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
	timehost.sin_port = htons(0);
	clnt->cl_auth = authdes_create("unix.500@homenet", 
				       60, 
				       (struct sockaddr *) &timehost,
				       (des_block *)NULL);

	/* Call the procedure twice to test nicknames */
	result_1 = add_1(a, b, clnt);
	if (result_1 == NULL) {
		clnt_perror(clnt, "1st call failed:");
		exit(1);
	}
	result_1 = add_1(a, b, clnt);
	if (result_1 == NULL) {
		clnt_perror(clnt, "2nd call failed:");
		exit(1);
	}
	clnt_destroy( clnt );
	return *result_1;
}


main( int argc, char* argv[] ) {
	char *host;
	int r;

	if(argc < 2) {
		printf("usage: %s server_host\n", argv[0]);
		exit(1);
	}
	host = argv[1];
	r = radd( host, 3, 4 );
	printf ("3 + 4 = %d\n", r);
	exit(0);
}
