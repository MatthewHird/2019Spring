#include <stdio.h>

int main()
{
   double d = 0.1;
   double sum = 0;

   printf("lab7c: ");
   while (sum != 1) {
      sum += d;
      printf("%lg ", sum);
      if (sum > 1.5) break;
   }
   printf("\n");

}

