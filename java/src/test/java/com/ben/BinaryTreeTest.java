package com.ben;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

import org.junit.Assert;
import org.junit.Test;

public class BinaryTreeTest {

	@Test
	public void testAdd() {
		BinaryTree bt = new BinaryTree();
		Random r = new Random();
		for(int i = 0; i < 50; i++) {
			bt.add(new BigDecimal(r.nextInt(40)));
		}
		
	}
	
	@Test
	public void testDepth() {
		BinaryTree bt = new BinaryTree();
		Assert.assertEquals(0, bt.getDepth());
		
		bt.add(new BigDecimal(35));
		Assert.assertEquals(1, bt.getDepth());
		
		bt.add(new BigDecimal(30));
		Assert.assertEquals(2, bt.getDepth());
		
		bt.add(new BigDecimal(49));
		Assert.assertEquals(2, bt.getDepth());
		
		bt.add(new BigDecimal(32));
		Assert.assertEquals(3, bt.getDepth());
		
		bt.add(new BigDecimal(15));
		Assert.assertEquals(3, bt.getDepth());
		
		bt.add(new BigDecimal(63));
		Assert.assertEquals(3, bt.getDepth());
		
		bt.add(new BigDecimal(78));
		Assert.assertEquals(4, bt.getDepth());
		
		bt.add(new BigDecimal(150));
		Assert.assertEquals(5, bt.getDepth());
		
		bt.add(new BigDecimal(70));
		Assert.assertEquals(5, bt.getDepth());
	}
	
	@Test
	public void testContains() {
	    List<BigDecimal> addedDec = new ArrayList<>();
        BinaryTree bt = new BinaryTree();
        Random r = new Random();
        
        Assert.assertFalse(bt.contains(new BigDecimal(3.5)));
        
        for(int i = 0; i < 200; i++) {
            int rInt = r.nextInt();
            BigDecimal bd = new BigDecimal(rInt);
            BigDecimal bdNotInTree = new BigDecimal(r.nextInt());
            
            addedDec.add(bd);
            bt.add(bd);
            
            for(BigDecimal found : addedDec) {
                Assert.assertTrue(bt.contains(found));
                Assert.assertFalse(bt.contains(bdNotInTree));
            }
        }
        
        for(int i = 0; i < 5; i++) {
            Collections.shuffle(addedDec);
            for(BigDecimal found : addedDec) {
                Assert.assertTrue(bt.contains(found));
            }
        }
        
	}

}
