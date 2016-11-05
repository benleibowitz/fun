package com.ben.algofun;

import org.junit.Test;

import java.math.BigInteger;

import static org.junit.Assert.assertEquals;

/**
 * Created by ben on 11/4/16.
 */
public class FibTest {
    @Test
    public void testFib() {
        assertEquals(BigInteger.ZERO, Fib.calcFib(1));
        assertEquals(BigInteger.ONE, Fib.calcFib(2));
        assertEquals(BigInteger.ONE, Fib.calcFib(3));
        assertEquals(new BigInteger("2"), Fib.calcFib(4));
        assertEquals(new BigInteger("3"), Fib.calcFib(5));
        assertEquals(new BigInteger("5"), Fib.calcFib(6));
        assertEquals(new BigInteger("8"), Fib.calcFib(7));
        assertEquals(new BigInteger("13"), Fib.calcFib(8));
        assertEquals(new BigInteger("21"), Fib.calcFib(9));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testFibWith0() {
        Fib.calcFib(0);
    }
}
