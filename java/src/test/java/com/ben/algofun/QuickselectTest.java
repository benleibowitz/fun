package com.ben.algofun;

import org.junit.Test;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

/**
 * Unit test for {@link Quicksort}.
 */
public class QuickselectTest {
    @Test(expected = IllegalArgumentException.class)
    public void testQuickSelectWithNullArray() {
        Quickselect.select(null, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testQuickSelectWithEmptyArray() {
        Integer[] array = new Integer[]{};
        Quickselect.select(array, 0);
    }

    @Test
    public void testQuickSelectWithArrayLength1() {
        Integer[] array = new Integer[]{4};
        assertEquals(4, (int)Quickselect.select(array, 1));
    }

    @Test
    public void testQuickSelect() {
        Integer[] array = new Integer[]{1, 3, 2};
        assertEquals(2, (int)Quickselect.select(array, 2));
        assertEquals(1, (int)Quickselect.select(array, 1));
        assertEquals(3, (int)Quickselect.select(array, 3));

        Integer[] array2 = new Integer[]{1, 3, 2, 4, 5, 23, 3};
        assertEquals(1, (int)Quickselect.select(array2, 1));
        assertEquals(2, (int)Quickselect.select(array2, 2));
        assertEquals(3, (int)Quickselect.select(array2, 3));
        assertEquals(3, (int)Quickselect.select(array2, 4));
        assertEquals(4, (int)Quickselect.select(array2, 5));
        assertEquals(5, (int)Quickselect.select(array2, 6));
        assertEquals(23, (int)Quickselect.select(array2,7));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testEmptyArrayThrowsException() {
        Integer[] array = new Integer[]{};
        Quickselect.select(array, 1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testIdxOutOfBoundsThrowsException() {
        Integer[] array = new Integer[]{1, 2, 3};
        Quickselect.select(array, 4);
    }
}
