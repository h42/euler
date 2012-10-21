#include <stdio.h>
#include <vector>
#include "primes.h"

using namespace std;

void tprimes() {
    int i;
    primes p;
    for (i=0;i<1000;i++) {
	if (p.isprime(i)) printf("%d is prime\n",i);
    }
    printf("get %d\n",p[6]);
    printf("get %d\n",p[100000]);
}

void pfactors(long n,vector<long> &pfv) {
    int i;
    long p;
    primes ps;
    for (i=0;; i++) {
	p=ps[i];
	if (p*2 > n) break;
	if (n%p==0) pfv.push_back(p);
    }
}

void tpfactors() {
    vector<long> pfv;
    pfactors(42,pfv);
    for (int i=0; i<(int)pfv.size(); i++) printf("%ld\n",pfv[i]);
}

int main() {
    //tprimes();
    tpfactors();
    return 0;
}
