#include <stdio.h>
#include <limits.h>

int main()
{
   int i = INT_MAX/64;
   float f = i;
   int j = f;
   printf("lab7d: i %d, j %d\n", i, j);
}

