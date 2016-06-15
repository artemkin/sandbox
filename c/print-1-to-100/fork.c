
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>

#define F fork(); wait(NULL);

int main(void)
{
    int* i = mmap(NULL, sizeof(int), PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    F F F F F F F

    if (*i < 100) printf("%d\n", ++*i);

    return 0;
}

