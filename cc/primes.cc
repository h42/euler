#include <stdio.h>
#include <vector>
#include <algorithm>

using namespace std;

static vector<long> zprimes; //shared by all classes

//ccinclude
class primes {
public:
    primes();
    bool isprime(int);
    int operator[](int x);
private:
    int next();
};
//ccinclude

primes::primes() {
    if (zprimes.size()==0) {
	zprimes.push_back(2);
	zprimes.push_back(3);
    }
}

bool primes::isprime(int x) {
    int i,p;
    if (x<4) return x<2 ? false : true;
    if (x<=zprimes[zprimes.size()-1]) {
	if (x%2==0 || x%3==0 || x%5==0 || x%7==0) return false;
	return binary_search(zprimes.begin(),zprimes.end(),x);
    }
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
