#include <stdarg.h>
#include <stdio.h>

// given the first parameter specifies how many additional
//    parameters are supplied,
// find and return the largest of the remaining parameters
double max(int numArgs, ...);

int main()
{
   double n;
   n = max(4, 0.1, -7, 200, 12);
   printf("Max of 0.1, -7, 200, and 12 is %lf\n", n);
   n = max(1, 1002.6);
   printf("Max of 1002.6 is %lf\n", n);
   n = max(3, 5, 10,0);
   printf("Max of 5, 10, and 0 is %lf\n", n);
}

// given the first parameter specifies how many additional
//    parameters are supplied,
// find and return the largest of the remaining parameters
double max(int numArgs, ...)
{ 
   // initialize the argument list
   va_list argList;
   va_start(argList, numArgs);

   double biggest;

   // get the first value from the argument list,
   //     treating it as a double
   biggest = va_arg(argList, double);

   // get the remaining values from the argument list,
   //     again treating them as doubles
   for (int i = 1; i < numArgs; i++) {
       double n = va_arg(argList, double);
       if (n > biggest) {
          biggest = n;
       }
   }

   // dispose of the argument list
   va_end(argList);
  
   // return the identified maximum
   return biggest;
}

