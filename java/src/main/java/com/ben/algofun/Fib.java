package com.ben.algofun;

import java.math.BigInteger;

public class Fib {
	private static BigInteger[] M;

    private static BigInteger TWO = new BigInteger("2");

    /**
     * Calculate fibonacci terms using dynamic programming.
     * @param n - the nth term to calculate, starting at 1
     * @return
     */
	public static BigInteger calcFib(int n) {
        if(n <= 0) {
            throw new IllegalArgumentException("n must be > 0");
        }
		M = new BigInteger[n + 1];
		return calcFib(new BigInteger(String.valueOf(n)));
	}

	private static BigInteger calcFib(BigInteger n) {
		if(n.equals(BigInteger.ONE)) {
			return BigInteger.ZERO;
		} else if(n.equals(TWO)) {
			return BigInteger.ONE;
		} else if(M[n.intValue()] != null) {
			return M[n.intValue()];
		} else {
			M[n.intValue()] = calcFib(n.subtract(BigInteger.ONE)).add(calcFib(n.subtract(new BigInteger("2"))));
			return M[n.intValue()];
		}
	}
}
