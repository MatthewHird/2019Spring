#include <cstdio>
#include "list.h"

int main()
{
   nodePtr front;
   std::string k, v;

   // try an insert into empty list
   insert(front, "first", "1");

   // try an insert into non-empty list
   insert(front, "second", "2");

   // try removing from non-empty list
   remove(front, k, v);
   printf("...removed %s:%s\n", k.c_str(), v.c_str());

   // insert again, to check that remove didn't mess up ptrs
   insert(front, "third", "3");

   // print list, should print third then first
   printf("The current list contents are:\n");
   print(front);

   // deallocate entire list
   printf("The list elements as they are deallocated are:\n");
   deallocate(front);

   // a second try to print should not print anything, list should be empty
   printf("The list contents at the end are:\n");
   print(front);
}

