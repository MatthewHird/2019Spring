#ifndef LIST_H
#define LIST_H
#include "node.h"

// print displays the key and value from each element
//    in the list, from front to back
void print(wnodePtr front);

// insert creates a new node, with data values k and v,
//    and inserts it at the front of the list (if the list
//    was previously empty then this becomes the only element
//    in the list, otherwise the "old" front becomes the
//    second element in the updated list)
void insert(nodePtr &front, std::string k, std::string v);

// if given a pointer to the front of a non-empty list,
//    remove extracts the front element from the list,
//    copies its data into parameters k and v,
//    updates front to refer to the "next" element,
//    and "resets" the removed pointer
// remove has no effect on an empty list
void remove(nodePtr &front, std::string &k, std::string &v);

// deallocate removes all the elements of the list,
//    from front to back, printing the
//    contents of each removed node as it goes
// deallocate has no effect on an empty list
void deallocate(nodePtr &front);

// NOTE: deallocate, remove, and insert each assume they are
//    being passed a pointer to the front element of a list,
//    their behaviour is unspecified if they are passed a
//    pointer to a list element other than the front.

#endif
