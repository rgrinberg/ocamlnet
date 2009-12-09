/* Same in C */

#include <unistd.h>
#include <time.h>
#include <sys/time.h>
#include <stdio.h>
#include <stdlib.h>

int main () {
    char *c;
    int k,p;
    struct timeval t0, t1;
    double d0, d1;
    unsigned int i;
    volatile unsigned int j;

    c = (char *) malloc(40000000);
    for (k=0; k < 9999999; k++) {
	p = 4*k;
	c[p] = 0x10;
	c[p+1] = 0x11;
	c[p+2] = 0x12;
	c[p+3] = 0x13;
    }

    gettimeofday(&t0, NULL);
    k = 0;
    while (k < 40000000) {
	i = * ((unsigned int *) (c+k));
	k += 4;
    }
    gettimeofday(&t1, NULL);
    j = i;  /* prevent that the above loop is optimized away! */

    d0 = t0.tv_sec + 0.000001 * t0.tv_usec;
    d1 = t1.tv_sec + 0.000001 * t1.tv_usec;

    printf("Time: %f\n", d1-d0);

    return 0;
}
