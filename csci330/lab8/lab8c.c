#include <stdio.h>

int f(int *a)
{
   (*a) *= 2;
   return (3 * (*a)) / 2;
}

int p(int a, int b, int c, int d)
{
    return a*b*c*d;
}

int main()
{
   int x = 1, y = 10, z = 100;
   printf("lab8c: ");
   int r = p(x++, ++y, f(&z), z/y);
   printf("   x %d, y %d, z %d, r %d\n", x, y, z, r);
}

