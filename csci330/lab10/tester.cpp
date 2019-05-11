#include <cstdio>
#include <cstring>
#include "lab10.h"

// returns true if x < y
bool comp(float x, float y) {
    return ((y-x)>0);
}

int main() {
    const int size = 6;
    float data[size] = { 3, 5.1, 1, -3.5, 8, 3 };
    ssort(data, size, comp);
    printf("Sorted data:");
    for (int i = 0; i < size; i++) printf(" %g", data[i]);
    printf("\n");
}
