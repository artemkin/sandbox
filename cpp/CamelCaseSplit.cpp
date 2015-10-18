
#include <iostream>
#include <regex>


int main()
{
   const std::string input = "CamelCaseString";

   // It seems C++11 doesn't support lookaround :(
   std::regex rgx("(?<=[a-z])(?=[A-Z])");

   std::sregex_token_iterator it(input.begin(), input.end(), rgx, -1);
   std::sregex_token_iterator end;

   for (; it != end; ++it)
      std::cout << *it << std::endl;

   return 0;
}

