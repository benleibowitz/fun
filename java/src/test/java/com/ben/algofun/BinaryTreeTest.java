package com.ben.algofun;

import com.ben.datastructures.BinaryTree;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * Created by Ben on 11/5/2016.
 */
public class BinaryTreeTest {
    @Test
    public void testAdd() {
        BinaryTree<Integer> tree = new BinaryTree<>();
        assertEquals(0, tree.getDepth());

        tree.add(3);
        assertTrue(tree.contains(3));
        assertFalse(tree.contains(2));
        assertFalse(tree.contains(4));
        assertEquals(1, tree.getDepth());

        tree.add(5);
        assertTrue(tree.contains(3));
        assertTrue(tree.contains(5));
        assertFalse(tree.contains(4));
        assertFalse(tree.contains(2));
        assertFalse(tree.contains(6));
        assertEquals(2, tree.getDepth());

        tree.add(4);
        assertTrue(tree.contains(3));
        assertTrue(tree.contains(5));
        assertTrue(tree.contains(4));
        assertFalse(tree.contains(2));
        assertFalse(tree.contains(6));
        assertEquals(3, tree.getDepth());

        tree.add(2);
        assertTrue(tree.contains(3));
        assertTrue(tree.contains(5));
        assertTrue(tree.contains(4));
        assertTrue(tree.contains(2));
        assertFalse(tree.contains(6));
        assertFalse(tree.contains(1));
        assertEquals(3, tree.getDepth());
    }
}
