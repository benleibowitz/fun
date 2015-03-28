/*
 * Implementation of Binary Search in Java, using
 * iteration (search()), and recursion (recursiveSearch()).
 * Both have been unit tested, and throw IllegalArgumentExceptions
 * if null has been passed into function as the input array.
 */

package com;

public class BinarySearch {
	public static int search(int[] inArray, int searchKey) {
		int foundIdx = -1;

		if(inArray == null)
			throw new IllegalArgumentException("Null input array");
		
		if(inArray.length > 0) {
			int lowerIdx = 0;
			int upperIdx = inArray.length - 1;
			int midIdx;
			boolean found = false;
			
			while(lowerIdx <= upperIdx && !found) {
				
				midIdx = (lowerIdx + upperIdx +1) / 2;
				
				if(inArray[midIdx] > searchKey)
					upperIdx = midIdx - 1;
				else if(inArray[midIdx] < searchKey)
					lowerIdx = midIdx + 1;
				else {
					foundIdx = midIdx;
					found = true;
				}
			} 
		
		}
		
		return foundIdx;
	}
	
	public static int recursiveSearch(int[] inArray, int searchKey) {
		if(inArray == null)
			throw new IllegalArgumentException("Null input array");
		
		return recursiveSearch(inArray, 0, inArray.length - 1, searchKey);
	}
	
	private static int recursiveSearch(int[] inArray, int lowerIdx, int upperIdx, int searchKey) {
		int foundIdx = -1;
		
		if(lowerIdx <= upperIdx) {
	        int midIdx = (lowerIdx + upperIdx) / 2;
	        
			if(inArray[midIdx] > searchKey)
				foundIdx = recursiveSearch(inArray, lowerIdx, midIdx - 1, searchKey);
			else if(inArray[midIdx] < searchKey)
				foundIdx = recursiveSearch(inArray, midIdx + 1, upperIdx, searchKey);
			else
				foundIdx = midIdx;
		}
		
		return foundIdx;
	}
	
}
