
// https://www.quora.com/How-would-you-write-a-C-program-to-print-1-to-100-without-loop-recursion-and-goto

#ifdef GENERATE

printf("%d\n", ++i);
#if __COUNTER__ < 99
    #include __FILE__
#endif

#else

#include <stdio.h>

int main(void)
{
    int i = 0;
    #define GENERATE
    #include __FILE__
    return 0;
}

#endif

