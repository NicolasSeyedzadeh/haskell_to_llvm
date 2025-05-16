#include <stdio.h>

int check_div(int x,int y){
    if (y==0 || y==-1){
        return -1;
    }
    else{
        return x/y;
    }
}

int main() {

    printf("%d\n",check_div(25,check_div(40,8)));

    return 0;
}
