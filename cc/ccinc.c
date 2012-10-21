#include <stdio.h>
#include <string.h>

int main() {
    int state=0;
    char buf[256];
    while (fgets(buf,sizeof(buf),stdin)) {
	if (!memcmp(buf,"//ccinclude",11)) {
	    if (++state==2) break;
	    continue;;
	}
	if (state) fputs(buf,stdout);
    }
    return 0;
}
