#include <stdio.h>

void pr(int a)
{
   printf("(%d)",a);
}

int main()
{
   int x = 10, y = 100;
   int z = (x++,x) * (++y,y);
   printf("x %d, y %d, z %d\n", x, y, z);
}

