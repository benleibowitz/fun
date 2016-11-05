package com.ben.algofun;

import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

/**
 * Created by ben on 10/29/16.
 */
public class FindKMinTest {
    @Test
    public void testFindKMin() {
        Integer[] result = KMin.findKMin(new Integer[]{4,2,3,6,2}, 1);
        Integer[] expected = new Integer[]{2};
        assertArrayEquals(expected, result);

        result = KMin.findKMin(new Integer[]{4,2,3,6,2}, 2);
        expected = new Integer[]{2, 2};
        assertArrayEquals(expected, result);

        result = KMin.findKMin(new Integer[]{4,2,3,6,2}, 3);
        expected = new Integer[]{2, 2, 3};
        assertArrayEquals(expected, result);

        result = KMin.findKMin(new Integer[]{4,2,3,6,2}, 4);
        expected = new Integer[]{2, 2, 3, 4};
        assertArrayEquals(expected, result);

        result = KMin.findKMin(new Integer[]{4,2,3,6,2}, 5);
        expected = new Integer[]{2, 2, 3, 4, 6};
        assertArrayEquals(expected, result);

        result = KMin.findKMin(new Integer[]{4,2,3,6,2}, 6);
        expected = new Integer[]{2, 2, 3, 4, 6};
        assertArrayEquals(expected, result);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testThrowsExceptionWithKEqual0() {
        KMin.findKMin(new Integer[]{4,2,3,6,2}, 0);
    }
}
