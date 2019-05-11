#include <stdio.h>

int main()
{
   int x = 10, y = 100;
   int z = x++ * ++y;
   printf("x %d, y %d, z %d\n", x, y, z);
}

