package com.ben.datastructures;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class MyStackTest {
    private MyStack<String> myStack;
    private Stack<String> jStack;
    
    private MyStack rawMyStack;
    private Stack rawJStack;
    
    private String[] testAr = new String[]{"foo", "bar", "abc", "def", "g", "h", "i",
            null, "j", null, "kllkjr", new String(), new String(""), ""};
    private List rawList;
    
    @Before
    public void setUp() throws Exception {
        myStack = new MyStack();
        jStack = new Stack<>();
        rawMyStack = new MyStack();
        rawJStack = new Stack();
        
        rawList = new ArrayList<>();
        rawList.add(3);
        rawList.add(new BigDecimal(35));
        rawList.add("foo bar");
        rawList.add(null);
        rawList.add(null);
        rawList.add(33.4);
        rawList.add(new int[]{});
        rawList.add(new int[]{3,5,7});
        List foo = new ArrayList<>();
        foo.add(3);foo.add(8);
        rawList.add(foo);
    }

    @After
    public void tearDown() throws Exception {
        myStack = null;
        jStack = null;
        rawMyStack = null;
        rawJStack = null;
    }

    @Test
    public void testAdd() {
        //Test parameterized
        assertEquals(0, myStack.getSize());
        
        for(String testStr : testAr) {
            jStack.add(testStr);
            myStack.add(testStr);
            assertEquals(jStack.size(), myStack.getSize());
        }
     
        //Test raw
        assertEquals(0, rawMyStack.getSize());
        for(Object obj : rawList) {
            rawMyStack.add(obj);
            rawJStack.add(obj);
            assertEquals(rawJStack.size(), rawMyStack.getSize());
        }
        
    }
    
    @Test
    public void testPop() {
        
        for(String testStr : testAr) {
            jStack.add(testStr);
            myStack.add(testStr);
        }
     
        for(Object obj : rawList) {
            rawMyStack.add(obj);
            rawJStack.add(obj);
        }
        
        while(!jStack.isEmpty()) {
            String jPop = jStack.pop();
            String myPop = myStack.pop();
            
            if(jPop != null && myPop != null)
                assertTrue(jPop.equals(myPop));
            else if(jPop == null ^ myPop == null)
                fail("Only 1 of pop outputs is null");
            
            assertEquals(jStack.size(), myStack.getSize());
        }

        while(!rawJStack.isEmpty()) {
            Object jPop = rawJStack.pop();
            Object myPop = rawMyStack.pop();
            
            if(jPop != null && myPop != null)
                assertTrue(jPop.equals(myPop));
            else if(jPop == null ^ myPop == null)
                fail("Only 1 of pop outputs is null");
            
            assertEquals(rawJStack.size(), rawMyStack.getSize());
        }
    }

    @Test(expected = ArrayIndexOutOfBoundsException.class)
    public void testPopEmptyArrayThrowsException() {
        myStack.pop();
    }

}
