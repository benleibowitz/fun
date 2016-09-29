package com.ben.algofun;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Unit test for {@link BinarySearch}.
 */
public class BinarySearchTest {
    @Test
    public void testEmptyArray() {
        assertEquals(-1, BinarySearch.search(new Integer[]{}, 3));
    }

    @Test
    public void testArrayLen1WhenFound() {
        assertEquals(0, BinarySearch.search(new Integer[]{3}, 3));
    }

    @Test
    public void testArrayLen1WhenNotFound() {
        assertEquals(-1, BinarySearch.search(new Integer[]{5}, 3));
        assertEquals(-1, BinarySearch.search(new Integer[]{1}, 3));
    }

    @Test
    public void testArrayWhenFoundAtRightIdx() {
        assertEquals(3, BinarySearch.search(new Integer[]{3, 4, 5, 7}, 7));
    }

    @Test
    public void testArrayWhenFoundAtLeftIdx() {
        assertEquals(0, BinarySearch.search(new Integer[]{3, 4, 5, 7}, 3));
    }

    @Test
    public void testArrayWhenFoundInMid() {
        assertEquals(2, BinarySearch.search(new Integer[]{3, 4, 5, 7, 9}, 5));
    }

    @Test
    public void testArrayWhenNotFound() {
        assertEquals(-1, BinarySearch.search(new Integer[]{3, 4, 5, 7, 9}, 6));
        assertEquals(-1, BinarySearch.search(new Integer[]{3, 4, 5, 7, 9}, 10));
        assertEquals(-1, BinarySearch.search(new Integer[]{3, 4, 5, 7, 9}, 2));
    }

}
