/* Install krb5

   gcc -lgssapi_krb5 -o exp_name exp_name.c
 */

#include <gssapi.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

int main(int argc, char ** argv) {
    OM_uint32 minor, major;
    gss_name_t name1, name2;
    gss_buffer_desc imp_name;
    gss_buffer_desc exp_name;
    gss_OID_set mechs;
    int k;
    char * s;

    imp_name.length = 4;
    imp_name.value = "gerd@realm";
    major = gss_import_name(&minor, &imp_name, GSS_C_NT_HOSTBASED_SERVICE, &name1);
    if (major != 0) {
	fprintf(stderr, "Error import: %d\n", major);
	exit(1);
    }
    major = gss_inquire_mechs_for_name(&minor, name1, &mechs);
    if (major != 0) {
	fprintf(stderr, "Error inquire: %d\n", major);
	exit(1);
    }
    if (mechs->count == 0) {
	fprintf(stderr, "No mechs\n");
	exit(1);
    };
    major = gss_canonicalize_name(&minor, name1, mechs->elements, &name2);
    if (major != 0) {
	fprintf(stderr, "Error canonicalize: %d\n", major);
	exit(1);
    }
    major = gss_export_name (&minor, name2, &exp_name);
    if (major != 0) {
	fprintf(stderr, "Error export: %d\n", major);
	exit(1);
    }
    fprintf(stderr, "All ok length=%d\n", (int) exp_name.length);
    s = (char *) exp_name.value;
    for (k=0; k<exp_name.length; k++) {
	fprintf(stderr, "\\x%02x", (unsigned char) s[k]);
    };
    fprintf(stderr, "\n");
    return 0;
}
