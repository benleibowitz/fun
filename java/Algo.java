/*
 *  A class in Java which contains a few basic algorithm functions
 *  Algo has the following functions, which modify arrays (* insertSort modifies array in place *)
 *  - void insertSort(int[]);
 *  - int[] mergeSort(int[]);
 */

public class Algo {
	
	public static int[] mergeSort(int[] inputArray) {
		int[] mergeSortedArray = inputArray;
		
		//if inputArray length <= 1, array is sorted and return array
		//if length > 1, sort recursively
		if(inputArray.length > 1) {
			
			//create L and R arrays from splitting inputArray
			int Llength = (int)inputArray.length / 2;
			int Rlength = inputArray.length - Llength;
			int[] Larray = new int[Llength];
			int[] Rarray = new int[Rlength];
			
			//fill our new L and R arrays
			for(int i=0; i<Llength; i++)
				Larray[i] = inputArray[i];
			for(int i=0; i<Rlength; i++)
				Rarray[i] = inputArray[i + Llength];
	
			//recursively split arrays into smaller subarrays
			Larray = mergeSort(Larray);
			Rarray = mergeSort(Rarray);
			
			mergeSortedArray = mergeTwoArrays(Larray, Rarray);
		}
		
		return mergeSortedArray;
	}
	
	private static int[] mergeTwoArrays(int[] Larray, int[] Rarray) {
		int[] outArray = new int[Larray.length + Rarray.length];
		int Lindex = 0;
		int Rindex = 0;
		
		for(int mainIndex = 0; mainIndex < outArray.length; mainIndex++) {
			boolean insertLval = false;
			boolean insertRval = false;
			
			if(Lindex < Larray.length && Rindex < Rarray.length) {
				int Rval = Rarray[Rindex];
				int Lval = Larray[Lindex];
				
				if(Lval < Rval)
					insertLval = true;
				else 
					insertRval = true;
				
			} else if(Lindex < Larray.length)
				insertLval = true;
			else if(Rindex < Rarray.length) 
				insertRval = true;
			
			if(insertLval)
				outArray[mainIndex] = Larray[Lindex++];
			else if(insertRval)
				outArray[mainIndex] = Rarray[Rindex++];
			
		}
		
		return outArray;
	}
	
	//sorts array in place
	public static void insertSort(int[] inArr) {
		
		//outer loop
		for(int i = 1; i < inArr.length; i++) {
			int key = inArr[i];
			
			//inner loop
			boolean inserted = false;
			for(int j = i; j > 0 && !inserted; j--) {
				int prevKey = inArr[j-1];
				
				if(prevKey > key) 
					inArr[j] = prevKey;
				else {
					inArr[j] = key;
					inserted = true;
				}
				
			}
			
			if(!inserted)
				inArr[0] = key;
			
		}//close outer loop
	}
}
