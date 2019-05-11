#include <stdio.h>

void f(void *p)
{
   int I = *(int*)(p);
   float F = *(float*)(p);
   printf("lab7b: I is %d, F is %g\n", I, F);
}

int main()
{
   int x = 3;
   f((void*)(&x));
   float y = 3;
   f((void*)(&y));
}

