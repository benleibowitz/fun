package com.ben.algofun;

import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertArrayEquals;

/**
 * Created by ben on 10/25/16.
 */
public class CountingsortTest {
    @Test(expected = IllegalArgumentException.class)
    public void testSortNullArray() {
        Countingsort.sort(null);
    }

    @Test
    public void testSortEmptyArray() {
        int[] x = {};
        int[] y = Arrays.copyOf(x, x.length);
        assertArrayEquals(y, Countingsort.sort(x));
    }

    @Test
    public void testSortLen1Array() {
        int[] x = {4};
        int[] y = Arrays.copyOf(x, x.length);
        assertArrayEquals(y, Countingsort.sort(x));
    }

    @Test
    public void testSort() {
        int[] x = {2,4,6,2,3,3,2,3,5,2,3,4,3,2,2,2,3,4,1,2,3};
        int[] y = Arrays.copyOf(x, x.length);
        Arrays.sort(y);
        assertArrayEquals(y, Countingsort.sort(x));
    }

    @Test
    public void testSortWithParams() {
        int[] x = {2,4,6,2,3,3,2,3,5,2,3,4,3,2,2,2,3,4,1,2,3};
        int[] y = Arrays.copyOf(x, x.length);
        Arrays.sort(y);
        assertArrayEquals(y, Countingsort.sort(x, 1, 6));
    }
}
