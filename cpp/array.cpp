// Trivial example of dependent type in C++

#include <iostream>
#include <array>

template<class T, size_t N1, size_t N2>
std::array<T, N1 + N2> append(const std::array<T, N1>& a1, const std::array<T, N2>& a2)
{
	std::array<T, N1 + N2> a;
	std::copy(a1.begin(), a1.end(), a.begin());
	std::copy(a2.begin(), a2.end(), a.begin() + N1);
	return a;
}

int main()
{
	std::array<int, 5> a1 { 1, 2, 3, 4, 5 };
	std::array<int, 3> a2 { 6, 7, 8 };
	
	auto result = append(a1, a2);
	
	for (auto& x : result)
		std::cout << x << " ";
		
	std::cout << std::endl;
	
	return 0;
}
