package com.ben.datastructures;

import org.apache.cassandra.index.Index;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;

public class MyLinkedListTest {
    private MyLinkedList<String> linkedList;
    private List<String> correctList;

    @Before
    public void setUp() throws Exception {
        linkedList = new MyLinkedList<>();
        correctList = new ArrayList<String>() {{
            add("foo");
            add("bar");
            add(null);
            add("woo");
        }};
    }

    @After
    public void tearDown() throws Exception {
    }

    @Test
    public void testSizeIs0WhenEmpty() {
        assertEquals(0, linkedList.getSize());
    }
    
    @Test
    public void testSizeIncrementsProperly() {
        linkedList.add("foo");
        assertEquals(1, linkedList.getSize());
        linkedList.add("foo");
        assertEquals(2, linkedList.getSize());
        linkedList.add(null);
        assertEquals(3, linkedList.getSize());
    }
    
    @Test
    public void testElementsAreAsExpected() {
        for(int i = 0; i < correctList.size(); i++) {
            String element = correctList.get(i);
            linkedList.add(element);
            for(int j = 0; j <= i; j++) {
                assertEquals(linkedList.get(j), correctList.get(j));
            }
        }
    }
    
    @Test
    public void testClearSizeIs0() {
        linkedList.add("foo");
        linkedList.add("bar");
        linkedList.clear();
        assertEquals(0, linkedList.getSize());
    }
    
    @Test(expected = IndexOutOfBoundsException.class)
    public void testGetWhenEmptyThrowsException() {
        linkedList.get(0);
    }

    @Test(expected = IndexOutOfBoundsException.class)
    public void testGetOutOfBoundsThrowsException() {
        linkedList.add("foo");
        linkedList.get(1);
    }
    
    @Test
    public void testContainsWhenDoesnt() {
    	linkedList.add("foobar");
    	assertFalse(linkedList.contains("Wu Tang"));
    }
    
    @Test
    public void testContainsWhenDoes() {
    	linkedList.add("tom jones");
    	linkedList.add("foo bar");
    	assertTrue(linkedList.contains("foo bar"));
    }
    
    @Test
    public void testContainsNullWhenDoes() {
    	linkedList.add("tom jones");
    	linkedList.add(null);
    	linkedList.add("woo");
    	assertTrue(linkedList.contains(null));
    }
    
    @Test
    public void testContainsNullWhenDoesnt() {
    	linkedList.add("foo");
    	linkedList.add("fodo");
    	assertFalse(linkedList.contains(null));
    }

    @Test
    public void testRemoveFirst() {
        linkedList.add("foo");
        linkedList.add("bar");
        linkedList.remove(0);
        assertEquals(linkedList.get(0), "bar");
        assertEquals(linkedList.getSize(), 1);
    }

    @Test
    public void testRemoveElementInMiddle() {
        linkedList.add("foo");
        linkedList.add("bar");
        linkedList.add("merp");
        linkedList.remove(1);
        assertEquals(linkedList.get(0), "foo");
        assertEquals(linkedList.get(1), "merp");
        assertEquals(linkedList.getSize(), 2);
    }

    @Test
    public void testRemoveLastElement() {
        linkedList.add("foo");
        linkedList.add("bar");
        linkedList.add("merp");
        linkedList.remove(2);
        assertEquals(linkedList.get(0), "foo");
        assertEquals(linkedList.get(1), "bar");
        assertEquals(linkedList.getSize(), 2);
    }

    @Test(expected = IndexOutOfBoundsException.class)
    public void testThrowsExceptionWhenRemoveOutOfBoundsEmpty() {
        linkedList.remove(0);
    }

    @Test(expected = IndexOutOfBoundsException.class)
    public void testThrowsExceptionWhenRemoveOutOfBoundsNotEmpty() {
        linkedList.add("foo");
        linkedList.remove(1);
    }

    @Test
    public void testToStringEmpty() {
        assertEquals(linkedList.toString(), "[]");
    }

    @Test
    public void testToStringNotEmpty() {
        linkedList.add("foo");
        linkedList.add("bar");
        assertEquals(linkedList.toString(), "[foo, bar]");
    }
}
