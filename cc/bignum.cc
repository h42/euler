#include <stdio.h>

int main() {
    int t=0;
    for (int i=3; i<1000; i++) {
	if (i%3==0 || i%5==0) t+=i;
    }
    printf ("sum = %d\n",t);
    return 0;
}
