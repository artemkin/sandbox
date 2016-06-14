
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char* argv[])
{
    int i = argc < 2 ? 1 : atoi(argv[1]);
    printf("%d\n", i);
    fflush(stdout);
    char buf[32];
    sprintf(buf, "%d", ++i);
    if (i <= 100)
        execlp(argv[0], argv[0], buf, NULL);
    return 0;
}

