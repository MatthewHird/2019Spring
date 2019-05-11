#ifndef LAB10_H
#define LAB10_H

template <class T>
void swap(T &x, T &y) {
    T z = x;
    x = y;
    y = z;
}

template <class T>
void ssort(T arr[], int size);

template <class T>
void ssort(T arr[], int size, bool(*compare)(T, T));


#endif
