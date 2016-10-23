package com.ben.algofun;

import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

/**
 * Unit test for {@link Quicksort}.
 */
public class QuickselectTest {
    @Test
    public void testQuickSelectWithArrayLength1() {
        int[] array = new int[]{4};
        assertEquals(4, Quickselect.select(array, 1));
    }

    @Test
    public void testQuickSelect() {
        int[] array = new int[]{1, 3, 2};
        assertEquals(2, Quickselect.select(array, 2));
        assertEquals(1, Quickselect.select(array, 1));
        assertEquals(3, Quickselect.select(array, 3));

        int[] array2 = new int[]{1, 3, 2, 4, 5, 23, 3};
        assertEquals(1, Quickselect.select(array2, 1));
        assertEquals(2, Quickselect.select(array2, 2));
        assertEquals(3, Quickselect.select(array2, 3));
        assertEquals(3, Quickselect.select(array2, 4));
        assertEquals(4, Quickselect.select(array2, 5));
        assertEquals(5, Quickselect.select(array2, 6));
        assertEquals(23, Quickselect.select(array2,7));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testEmptyArrayThrowsException() {
        int[] array = new int[]{};
        Quickselect.select(array, 1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testIdxOutOfBoundsThrowsException() {
        int[] array = new int[]{1, 2, 3};
        Quickselect.select(array, 4);
    }
}
