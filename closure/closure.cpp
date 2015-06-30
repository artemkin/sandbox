
#include <vector>
#include <iostream>
#include <memory>

typedef std::function<int()> func;

std::vector<func> generate() {
   std::vector<func> result;

   auto counter = std::make_shared<int>(0);

   for (int i = 0; i < 10; ++i) {
      auto increment = [=]() {
         return (*counter)++;
      };
      result.push_back(increment);
   }

   return result;
}

int main() {
   auto seq = generate();
   auto seq2 = seq;

   for (auto& s : seq) {
      std::cout << s() << std::endl;
   }

   for (auto& s : seq2) {
      std::cout << s() << std::endl;
   }

   return 0;
}

