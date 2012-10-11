#include <stdio.h>
#include <math.h>

const int pmax = 100000;
long primes[pmax],plen=0;
long bigboy = 600851475143,bb2=sqrt(bigboy);

void loadprimes () {
    int i,j,p,p2,prime;
    plen=0;
    primes[plen++]=2;
    primes[plen++]=3;
    for (i=5;plen<pmax-1;i+=2) {
	prime=1;
	p2 = sqrt(i);
	for (j=0;j<plen;j++) {
	    p=primes[j];
	    if (p>p2) break;
	    if (i%p==0) {
		prime=0;
		break;
	    }
	}
	if (prime) primes[plen++]=i;
    }
    return;
}

int main() {
    loadprimes();
    printf("prime %ld = %ld\n",plen-1,primes[plen-1]);
    return 0;
}
