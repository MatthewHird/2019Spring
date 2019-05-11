#include <stdio.h>
#include <stdlib.h>

const int GlobalConst = 0; //
int GlobalVar = 1; //

void f(int param) //
{
    printf("%s: %p\n", "GlobalConst", &GlobalConst);
    printf("%s: %p\n", "GlobalVar", &GlobalVar);

    static int staticVar = 2;
    int localVar = 3;
    const int localConst = 4;

    printf("%s: %p\n", "staticVar", &staticVar);
    printf("%s: %p\n", "localVar", &localVar);
    printf("%s: %p\n", "localConst", &localConst);


    for (int loopVar = 5; loopVar <= 5; loopVar++) { 
        const int loopConst = 6; 

        printf("%s: %p\n", "loopVar", &loopVar);
        printf("%s: %p\n", "loopConst", &loopConst);
    }

    int *ptr = (int*)malloc(param*sizeof(int));
    if (ptr) {
      *ptr = 7;
      printf("%s: %p\n", "ptr after val assign", ptr);
    }
    printf("%s: %p\n", "ptr after if", ptr);
    free(ptr); 
    printf("%s: %p\n", "ptr after free", ptr);
}

int main(int argc, char *argv[])
{
    printf("%s: %p\n", "argc", &argc);
    printf("%s: %p\n", "argv", &argv);

    int mainVar = 8; 
    int mainConst = 9;

    printf("%s: %p\n", "mainVar", &mainVar);
    printf("%s: %p\n", "mainConst", &mainConst);


    f(argc);
}
