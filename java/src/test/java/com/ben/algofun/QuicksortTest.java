package com.ben.algofun;

import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertArrayEquals;

/**
 * Unit test for {@link Quicksort}.
 */
public class QuicksortTest {
    @Test
    public void testSortEmptyArray() {
        sortAndAssertCorrect(new int[]{});
    }

    @Test
    public void testSortArraySize1() {
        sortAndAssertCorrect(new int[]{5});
    }

    @Test
    public void testSortArraySize2() {
        sortAndAssertCorrect(new int[]{5, 6});
        sortAndAssertCorrect(new int[]{5, 4});
        sortAndAssertCorrect(new int[]{5, 5});
    }

    @Test
    public void testSortArray() {
        sortAndAssertCorrect(new int[]{12, -5, 19, 5, 7, 5, 19, 0, 6, 6, 5});
    }

    private void sortAndAssertCorrect(int[] numbers) {
        int[] expectedSortedArray = Arrays.copyOf(numbers, numbers.length);

        int[] numbers2 = Arrays.copyOf(numbers, numbers.length);

        Arrays.sort(expectedSortedArray);
        Quicksort.sort(numbers);
        Quicksort.randomizedSort(numbers2);
        assertArrayEquals(expectedSortedArray, numbers);
        assertArrayEquals(expectedSortedArray, numbers2);


    }
}
