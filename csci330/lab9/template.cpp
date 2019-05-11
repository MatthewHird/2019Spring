#include <cstdio>
#include <string>

// varadic templates in C++ allow functions to take an arbitrary number
//    of parameters in a type-safe way
// the argument handling is resolved at compile time (unlike the C-style
//    va-args approach, which relies on run-time type casting and
//    hoping you passed the right types!)


// base case, just the required arguments
template <typename T>
T Sum(T x) {
    return x;
}

// general case, at least the required args and the typename
//    for the next arg, everything else is captured in the ... Args
template<typename T, typename... Args>
T Sum(T front, Args... args) {
   return front + Sum(args...);
}

int main()
{
   // this can now be called on any operands for which + is a valid operator
   int   itotal = Sum(1,2,3);
   printf("Sum(1,2,3) is %d\n", itotal);

   float ftotal = Sum(4.5, -3.2, 27.0);
   printf("Sum(4.5,-3.2,27.0) is %g\n", ftotal);

   std::string s1="hi ", s2="there!";
   std::string stotal = Sum(s1,s2);
   printf("Sum(%s,%s) is %s\n", s1.c_str(), s2.c_str(), stotal.c_str());
}

