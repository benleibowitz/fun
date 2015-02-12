/*
*  an insert sort implementation in C++
*/
#include <iostream>

void insertSort(int arr[], const unsigned int LENGTH) {
  
  if(LENGTH > 0) {
    for(int i=1; i<LENGTH; i++) {
    
      int key = arr[i];
      bool inserted = false;
      for(int j=i; j>0 && !inserted; j--) {
        int prevKey = arr[j-1];
        
        if(prevKey > key) 
	        arr[j] = prevKey;
        else {
          arr[j] = key;
	        inserted = true;
        }

        if(j == 1 && !inserted) arr[0] = key;
      }


    }
  }

}
 

int main() {
  const int L = 100;
  
  int foo[L];
  for(int i=0;i<L;i++) foo[i] = 200 - 2*i;

  insertSort(foo, L);

  return 0;
}
