
#include "consts.h"

#include <iostream>

void print()
{
    std::cout << "print: " << DEFINE_STRING << std::endl;
    std::cout << "print: " << const_string << std::endl;
    std::cout << "print: " << static_const_string << std::endl;
}

