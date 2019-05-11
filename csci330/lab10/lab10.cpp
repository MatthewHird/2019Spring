#include "lab10.h"

template <class T>
void ssort(T arr[], int size) {
    for (int i = 0; i < (size-1); i++) {
        T min = arr[i];
        int pos = i;
        for (int j = i+1; j < size; j++) {
            if (arr[j] < min) {
                min = arr[j];
                pos = j;
            }
         }
         if (pos != i) {
             swap(arr[i], arr[pos]);
         }
    }
}
template void ssort<float>(float arr[], int size);


template <class T>
void ssort(T arr[], int size, bool(*compare)(T, T)) {
    for (int i = 0; i < (size-1); i++) {
        T min = arr[i];
        int pos = i;
        for (int j = i+1; j < size; j++) {
            if ((*compare)(arr[j], min)) {
                min = arr[j];
                pos = j;
            }
         }
         if (pos != i) {
             swap(arr[i], arr[pos]);
         }
    }
}
template void ssort<float>(float arr[], int size, bool(*compare)(float, float));

