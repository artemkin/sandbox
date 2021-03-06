// C++14 compose function

#include <string>
#include <functional>
#include <iostream>

/*!!! terrible template stuff
template<typename T>
struct get_arg;

template<typename R, typename Arg>
struct get_arg<R(Arg)>
{
   using type = Arg;
};

template<typename R, typename Arg>
struct get_arg<R(*)(Arg)>
{
   using type = Arg;
};

template<typename G, typename F>
auto compose(G g, F f)
{
   using Arg = typename get_arg<F>::type;

   return [=](Arg x) { return g(f(x)); };
}
*/

const auto compose = [](auto g, auto f)
{
   return [=](auto x) { return g(f(x)); };
};

std::string f(int x)
{
   return std::to_string(x);
}

double g(const std::string& str)
{
   return std::stod(str);
}

int main()
{
   std::cout << compose([](double x) { return x / 2.0; }, compose(g, f))(5) << std::endl;
   return 0;
}

