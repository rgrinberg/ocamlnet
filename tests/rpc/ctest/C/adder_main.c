#1 "adder_main.c"
/* Originally created by rpcgen, but modified by hand */
/* This file will be appended to adder_svc.c */

#include <rpc/auth.h>
#include <rpc/auth_unix.h>

static void
auth_adder_1(struct svc_req *rqstp, register SVCXPRT *transp)
{
    struct authunix_parms *sys_cred;
    struct authdes_cred *des_cred;
    uid_t uid;
    gid_t gid;
    int gidlen;
    gid_t gidlist[16];

    /* authenticate and do the adder_1 service */

    /* do not authenticate NULLPROC: */
    if (rqstp->rq_proc == NULLPROC) {
	adder_1(rqstp, transp);
	return;
    };

    fprintf(stderr, "Auth flavor: %d\n", rqstp->rq_cred.oa_flavor);
    fflush(stderr);

    switch (rqstp->rq_cred.oa_flavor) {
    case AUTH_SYS:
	fprintf(stderr, "AUTH_SYS\n");
	fflush(stderr);
	sys_cred = (struct authunix_parms *) rqstp->rq_clntcred;
	uid = sys_cred->aup_uid;
	break;

    case AUTH_DES:
	fprintf(stderr, "AUTH_DES\n");
	fflush(stderr);
	des_cred = (struct authdes_cred *) rqstp->rq_clntcred;
	fprintf(stderr, "Successfully decrypted.\n");
	fflush(stderr);
	if (! netname2user( des_cred->adc_fullname.name, &uid,
			    &gid, &gidlen, gidlist)) {
	    fprintf(stderr, "unknown user: %s\n",
		    des_cred->adc_fullname.name);
	    /*	    svcerr_systemerr(transp);
		    return;
	    */
	}
	break;

    default:
	svcerr_weakauth(transp);
	return;
    }

    /* Debug output */
    fprintf(stderr, "Authenticated as: %d\n", uid);
    fflush(stderr);

    /* Continue with generated procedure dispatcher: */
    adder_1(rqstp, transp);
}


int
main(int argc, char **argv)
{
	register SVCXPRT *transp;

	(void) pmap_unset(ADDER, V1);

	transp = svcudp_create(RPC_ANYSOCK);
	if (transp == NULL) {
		fprintf(stderr, "cannot create udp service.");
		exit(1);
	}
	if (!svc_register(transp, ADDER, V1, auth_adder_1, IPPROTO_UDP)) {
		fprintf(stderr, "unable to register (ADDER, V1, udp).");
		exit(1);
	}

	transp = svctcp_create(RPC_ANYSOCK, 0, 0);
	if (transp == NULL) {
		fprintf(stderr, "cannot create tcp service.");
		exit(1);
	}
	if (!svc_register(transp, ADDER, V1, auth_adder_1, IPPROTO_TCP)) {
		fprintf(stderr, "unable to register (ADDER, V1, tcp).");
		exit(1);
	}

	svc_run();
	fprintf(stderr, "svc_run returned");
	exit(1);
	/* NOTREACHED */
}
