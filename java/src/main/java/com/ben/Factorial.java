/*
 *  
 *  Implementation for basic factorial methods-
 *  findFactorial() is the typical count-down factorial
 *  method, while findFactorialUp() starts at 1 and
 *  finds the factorial by increasing (ie, 1 * 2 * 3 * 4)
 */

package com.ben;

public class Factorial {
	public static long findFactorial(int num) {
		long factorial = 1;

		if(num < 0)
    			throw new IllegalArgumentException("Factorial with negative input is undefined");

		if(num <= 1)
			factorial = 1;
		else
			factorial = num * findFactorial(num - 1);
		
		return factorial;
	}
	
	public static long findFactorialUp(int num) {
		if(num < 0)
			throw new IllegalArgumentException("Factorial with negative input is undefined");
    
		//if number is greater than 1, find factorial, else 
		//number is 1 or 0, and return number.
		return (num > 1) ? findFactorialUp(1, num) : 1;
	}
	
	private static long findFactorialUp(int currentNum, int target) {
		long factorial = 1;

		if(currentNum == target) 
			factorial = target;
		else 
			factorial = currentNum * findFactorialUp(currentNum + 1, target);
		
		return factorial;
	}
	
}
