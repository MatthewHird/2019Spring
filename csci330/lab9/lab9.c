#include <stdarg.h>
#include <stdio.h>

void PrintLongs(int numArgs, ...);

int main() {
    printf("List of 1 long:\n");
    PrintLongs(1, 4234);
    printf("List of 3 longs:\n");
    PrintLongs(3, 1, 1002, 666333);
    printf("List of 5 longs:\n");
    PrintLongs(5, 3, 5, 10, 0, 2294934);
}

void PrintLongs(int numArgs, ...) { 
    va_list argList;
    va_start(argList, numArgs);

    for (int i = 0; i < numArgs; i++) {
         printf("%ld\n", va_arg(argList, long));
    }

    printf("\n");

    va_end(argList);
}
