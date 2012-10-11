#include <stdio.h>

int main() {
    int a=1,b=1,fib,t=0;
    while ((fib=a+b) < 4000000) {
	if ((fib)%2 == 0) t+=fib;
	a=b;
	b=fib;
    }
    printf("even fibs = %d\n", t);
    return 0;
}
