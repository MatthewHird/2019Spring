#include <cstdio>
#include <string>

// varadic templates in C++ allow functions to take an arbitrary number
//    of parameters in a type-safe way
// the argument handling is resolved at compile time (unlike the C-style
//    va-args approach, which relies on run-time type casting and
//    hoping you passed the right types!)


// base case, just the required arguments
template <typename T>
T PrintLongs(T x) {
    printf("%ld\n\n", x);
}

// general case, at least the required args and the typename
//    for the next arg, everything else is captured in the ... Args
template<typename T, typename... Args>
T PrintLongs(T front, Args... args) {
    printf("%ld\n", front);
    PrintLongs(args...);
}

int main()
{
    printf("List of 1 long:\n");
    PrintLongs(4234);
    printf("List of 3 longs:\n");
    PrintLongs(1, 1002, 666333);
    printf("List of 5 longs:\n");
    PrintLongs(3, 5, 10, 0, 2294934);
}
