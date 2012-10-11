#include <stdio.h>
#include <vector>

using namespace std;

static vector<int> zprimes; //shared by all classes

class primes {
public:
    primes();
    bool isprime(int);
    int operator[](int x);
private:
    int next();
};

primes::primes() {
    if (zprimes.size()==0) {
	zprimes.push_back(2);
	zprimes.push_back(3);
    }
}

bool primes::isprime(int x) {
    int i,p;
    if (x<4) return x<2 ? false : true;
    for (i=0;;i++) {
	p = (*this)[i]; //get function == operator[] would be more readable
	if (x%p==0) return false;
	if (p*p > x) return true;
    }
    return true; //dummy for warning
}

int primes::next() {
    int pos = (int)zprimes.size();
    for (int p=zprimes[pos-1]+2;;p+=2) {
	if (isprime(p)) {
	    zprimes.push_back(p);
	    return p;
	}
    }
    return 0;
}

int primes::operator[](int x) {
    int plen = (int)zprimes.size();
    if (plen > x) return zprimes[x];
    int y;
    while ((int)zprimes.size() <= x) y=next();
    return y;
}

int main() {
    int i;
    primes p;
    for (i=0;i<100;i++) {
	if (p.isprime(i)) printf("%d is prime\n",i);
    }
    printf("get %d\n",p[6]);
    printf("get %d\n",p[99999]);
    for (i=0;i<10;i++) printf("%d\n",p[i]);
    return 0;
}
