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
        Integer[] array = new Integer[]{4};
        assertEquals(4, Quickselect.select(array, 1).intValue());
    }

    @Test
    public void testQuickSelect() {
        Integer[] array = new Integer[]{1, 3, 2};
        assertEquals(2, Quickselect.select(array, 2).intValue());
        assertEquals(1, Quickselect.select(array, 1).intValue());
        assertEquals(3, Quickselect.select(array, 3).intValue());

        Integer[] array2 = new Integer[]{1, 3, 2, 4, 5, 23, 3};
        assertEquals(1, Quickselect.select(array2, 1).intValue());
        assertEquals(2, Quickselect.select(array2, 2).intValue());
        assertEquals(3, Quickselect.select(array2, 3).intValue());
        assertEquals(3, Quickselect.select(array2, 4).intValue());
        assertEquals(4, Quickselect.select(array2, 5).intValue());
        assertEquals(5, Quickselect.select(array2, 6).intValue());
        assertEquals(23, Quickselect.select(array2,7).intValue());
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
