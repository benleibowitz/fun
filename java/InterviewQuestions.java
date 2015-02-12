/*
 *	Below are some implementations of a few basic
 *	interview questions in Java
*/

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

//thrown by rotateArray function
class ArrayNotSquareException extends Exception {
	ArrayNotSquareException() {
	}
	ArrayNotSquareException(String message) {
		super(message);
	}
}

//runnable that counts number of occurances of each number in array
class IterateThread implements Runnable {
	int[] subArray;
	HashMap<Integer, Integer> numberCount;
	
	IterateThread() {
		numberCount = new HashMap<>();
	}
	
	IterateThread(int[] arrayIn) {
		subArray = arrayIn;
		numberCount = new HashMap<>();
	}
	
	@Override
	public void run() {
		for(int num : subArray) {
			if(!(numberCount.keySet().contains(num))) numberCount.put(num, 1);
			else numberCount.put(num, numberCount.get(num)+1);
		}
	}
	
	public HashMap<Integer, Integer> getElementsCount() {
		return numberCount;
	}
	
}

public class InterviewQuestions {
	static Random r = new Random();
	
	public static boolean arrayIsSquare(int[][] inputArray) {
		boolean square = true;
	    
		for(int i = 0; i < inputArray.length; i++) {
			if(inputArray[i].length != inputArray.length) 
				square = false;
		}
	    
	    	return square;
	}
	
	//rotates array clockwise
	public static int[][] rotateArray(int[][] inArray) throws ArrayNotSquareException {
		int[][] outArray;
		
		if(!arrayIsSquare(inArray))
			throw new ArrayNotSquareException();
		else {
			outArray = new int[inArray.length][inArray.length];

			//loop for each "ring/layer"
			//ie. if array is 5x5, we loop 3 times. to rotate the outer layer, next layer, and middle layer
			//if array is 4x4, we loop 2x, rotating the outer layer, then the middle layer
			for(int i = 0; i < ((inArray.length % 2 == 0) ? inArray.length / 2 : (inArray.length+1) / 2); i++) {

				//move top arrays to the right side
				for(int k = i; k < inArray.length - i; k++) 
					outArray[k][inArray.length - 1 - i] = inArray[i][k];

				//move left arrays to top
				for(int k = i; k < (inArray.length - 1 - i); k++) 
					outArray[i][k] = inArray[inArray.length - 1 - k][i];

				//move bottom arrays to the left
				for(int k = i; k < inArray.length - i; k++) 
					outArray[inArray.length - 1 - k][i] = inArray[inArray.length - 1 - i][inArray.length - 1 - k];

				//move right arrays to the bottom
				for(int k = inArray.length - i - 1; k >= i; k--) 
					outArray[inArray.length - 1 - i][k] = inArray[inArray.length - 1 - k][inArray.length - 1 - i];

			}
		}
		
		return outArray;
	}
	
	//test out my random number methods
	public static void testRandom() {
		Map<Integer, Integer> m = new HashMap<>();
		for(int i=0; i<1000000;i++) {
			int rr = random3_12();
			if(!m.containsKey(rr))
				m.put(rr, 1);
			else
				m.put(rr, m.get(rr)+1);
		}
		
		int totalFrequency = 0;
		for(int i : m.keySet()) {
			totalFrequency += m.get(i);
		}
		
		DecimalFormat df = new DecimalFormat("##.##");
		for(int i : m.keySet()) {
			System.out.println("Value:" + i + ", Percentage Occurance:" + df.format(100.0 * m.get(i) / totalFrequency));
		}
	}
	
	//using only a function that generates a random number 1-5,
	//and generate a random number 1-7
	public static int random1_7() {
		int output;
		
		do {
			output = 5 * random1to5() + random1to5();
		} while(output > 26);
		
		return(output % 7 + 1);
	}
	
	//using only a function that generates a random number 1-5,
	//and generate a random number 3-12 
	public static int random3_12() {
		int output;
		
		do {
			output = 5 * random1to5() + random1to5();
		} while(output > 15);
		
		return(output % 10 + 3);
	}
	
	public static int random1to5() {
		return(r.nextInt(5) + 1);
	}

	public static void testMultiThreadDistinctFind() {
		final long len = 350000000;
		final int shortLen = (int)Math.ceil(len / 3.0);
		
		int[] subArray1 = new int[shortLen];
		int[] subArray2 = new int[shortLen];
		int[] subArray3 = new int[shortLen];
		int[][] totalArray = {subArray1, subArray2, subArray3};
		for(int i=0; i<totalArray.length; i++) {
			for(int j=0; j<totalArray[i].length; j++) {
				totalArray[i][j] = 4+i;
				totalArray[i][4] = 2+i;
			}
		}
		
		IterateThread r1 = new IterateThread(totalArray[0]);
		IterateThread r2 = new IterateThread(totalArray[1]);
		IterateThread r3 = new IterateThread(totalArray[2]);
		r1.run();
		r2.run();
		r3.run();
		HashMap<Integer, Integer> count1 = r1.getElementsCount();
		HashMap<Integer, Integer> count2 = r2.getElementsCount();
		HashMap<Integer, Integer> count3 = r3.getElementsCount();
		for(int value : count1.keySet()) System.out.println("Value: " + value + " occurs: " + count1.get(value) + " times");
		for(int value : count2.keySet()) System.out.println("Value: " + value + " occurs: " + count1.get(value) + " times");
		for(int value : count3.keySet()) System.out.println("Value: " + value + " occurs: " + count1.get(value) + " times");
		
	}
	
	public static void testElementFind() {
		int[] foo = new int[35000];
		for(int i=0; i<35000; i++) 
			foo[i] = 23;
		foo[2] = 8;
		foo[5] = 12;
		findDistinctEle(foo);
	}
	
	public static boolean isPrime(int input) throws IllegalArgumentException {
		boolean prime = true;
		
		if(input < 2) 
			throw new IllegalArgumentException("Number must be >= 2");
		
		for(int i = (input - 1); i > 1; i--) {
			if(input % i == 0) 
				prime = false;
		}
		
		return prime;
	}
	
	public static void findDistinctEle(int[] arrayIn) {
		//hashmap will hold <Value, #timesValueOccursInArray>
		HashMap<Integer, Integer> numberCount = new HashMap<>();
		
		for(int num : arrayIn) {
			if(!(numberCount.keySet().contains(num))) 
				numberCount.put(num, 1);
			else 
				numberCount.put(num, numberCount.get(num)+1);
		}
		
		for(int num : numberCount.keySet()) {
			System.out.println(num + " count: " + numberCount.get(num));
		}
	}
	
	//attempts to find element in array where:
	//sum of elements on left side = sum elements on right side.
	//if can't find, return -1
	public static int findIndex(List<Integer> list) {
		int index = -1;
		
		boolean indexFound = false;
		for(int i=0; i<list.size() && !indexFound; i++) {
			int sumLess = 0;
			int sumGreater = 0;
			
			for(int j=0; j<i; j++) 
				sumLess += list.get(j);
			for(int k=(i+1); k<list.size(); k++) 
				sumGreater += list.get(k);
			
			if(sumLess == sumGreater) {
				indexFound = true;
				index = i;
			}
		}
		return index;
	}
	
	public static int findDuplicate(List<Integer> list) throws IllegalArgumentException {
		int duplicate = 0;
		List<Integer> notDuplicates = new ArrayList<>();
		List<Integer> duplicatesList = new ArrayList<>();
		
		for(int i : list) {
			int num = list.get(i);
			
			if(!notDuplicates.contains(num))
				notDuplicates.add(num);
			else 
				if(!duplicatesList.contains(num)) 
					duplicatesList.add(num);
			
		}
		
		if(duplicatesList.size() == 1) 
			duplicate = duplicatesList.get(0);
		else 
			throw new IllegalArgumentException("More than one duplicate in list");
		
		return duplicate;
	}
	
	public static int stringToInt(String stringIn) throws IllegalArgumentException {
		final char[] digits = {'0','1','2','3','4','5','6','7','8','9'};
		int intOut = 0;
		boolean numNegative;
		
		if(stringIn == null || stringIn.length() == 0 || stringIn.length() > 9) 
			throw new IllegalArgumentException("String cannot be null, 0 length, or greater than 9 digits");
		
		char[] stringArray = stringIn.toCharArray();
		
		if(stringArray[0] == '-') {
			numNegative = true;
			stringArray[0] = '0';
		}
		else numNegative = false;
		
		for(int i=0; i<stringArray.length; i++) {
			boolean numIsInt = false;
			for(int j=0; j<digits.length; j++) {
				if(digits[j] == stringArray[i]) 
					numIsInt = true;
			}
			
			if(!numIsInt)
				throw new IllegalArgumentException("Number must only contain numerical digits");
			
			int localNum = stringArray[i] - '0';
			
			intOut *= 10;
			intOut += localNum;
		}
		
		if(numNegative) 
			intOut *= -1;
		
		return intOut;
	}
	
}
