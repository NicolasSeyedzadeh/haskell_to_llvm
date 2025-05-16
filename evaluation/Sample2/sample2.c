#include <stdio.h>

int mult(int accumulator,int x,int y){
    if (y == 0){
        return accumulator;
    }
    else{
        return mult(accumulator+x,x,y-1);
    }
}

int main() {
  printf("%d\n",mult(0,12345,54321));
  return 0;
}
