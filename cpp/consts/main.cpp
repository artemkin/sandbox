
#include "consts.h"

#include <iostream>

extern void print();

int main()
{
    std::cout << DEFINE_STRING << std::endl;
    std::cout << const_string << std::endl;
    std::cout << static_const_string << std::endl;
    print();

    return 0;
}
