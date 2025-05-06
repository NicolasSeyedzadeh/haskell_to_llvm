#include <stdio.h>

int add(int x,int y){
    return x+y;
}

int main() {
    int arg1= add(0,0);
    int arg2= add(0,0);
    printf("%d\n",add(arg1,arg2));
    return 0;
}
